{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-| The chain index' version of a transaction
-}
module Plutus.ChainIndex.Tx(
    ChainIndexTx(..)
    , fromOnChainTx
    , txOutRefs
    , outputsMapFromTxForAddress
    -- ** Lenses
    , citxTxId
    , citxInputs
    , citxOutputs
    , citxValidRange
    , citxData
    , citxRedeemers
    , citxMintingPolicies
    , citxStakeValidators
    , citxValidators
    , citxIsValid
    ) where

import           Control.Lens              (makeLenses)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)
import           Ledger                    (Address, Datum, DatumHash, MintingPolicy, MintingPolicyHash, OnChainTx (..),
                                            Redeemer (..), RedeemerHash, SlotRange, StakeValidator, StakeValidatorHash,
                                            Tx (..), TxId, TxIn (txInType), TxInType (..), TxOut (txOutAddress),
                                            TxOutRef (..), Validator, ValidatorHash, datumHash, mintingPolicyHash,
                                            redeemerHash, txId, validatorHash)

data ChainIndexTx = ChainIndexTx {
    _citxTxId            :: TxId,
    -- ^ The id of this transaction.
    _citxInputs          :: Set TxIn,
    -- ^ The inputs to this transaction.
    _citxOutputs         :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    _citxValidRange      :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    _citxData            :: Map DatumHash Datum,
    -- ^ Datum objects recorded on this transaction.
    _citxRedeemers       :: Map RedeemerHash Redeemer,
    -- ^ Redeemers of the minting scripts.
    _citxMintingPolicies :: Map MintingPolicyHash MintingPolicy,
    -- ^ The scripts used to check minting conditions.
    _citxStakeValidators :: Map StakeValidatorHash StakeValidator,
    _citxValidators      :: Map ValidatorHash Validator,
    _citxIsValid         :: Bool
    -- ^ An invalid tx does not produce any outputs, and its inputs are collateral inputs
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

makeLenses ''ChainIndexTx

instance Pretty ChainIndexTx where
    pretty t@ChainIndexTx{_citxTxId, _citxInputs, _citxOutputs, _citxValidRange, _citxMintingPolicies, _citxData, _citxRedeemers} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList _citxInputs)))
                , hang 2 (vsep ("outputs:" : fmap pretty _citxOutputs))
                , hang 2 (vsep ("minting policies:": fmap (pretty . fst) (Map.toList _citxMintingPolicies)))
                , "validity range:" <+> viaShow _citxValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList _citxData) ))
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList _citxRedeemers) ))
                ]
            isValidS = if _citxIsValid t then "Valid:" else "Invalid:"
        in nest 2 $ vsep [isValidS <+> "tx" <+> pretty _citxTxId <> colon, braces (vsep lines')]

txOutRefs :: ChainIndexTx -> [(TxOut, TxOutRef)]
txOutRefs ChainIndexTx{_citxTxId, _citxOutputs} =
    map (\(output, idx) -> (output, TxOutRef _citxTxId idx)) $ zip _citxOutputs [0..]

-- | Get map of tx outputs from tx for specific address.
outputsMapFromTxForAddress :: Address -> ChainIndexTx -> Map TxOutRef TxOut
outputsMapFromTxForAddress addr tx
  | _citxIsValid tx = Map.filter ((==) addr . txOutAddress)
                    $ Map.fromList
                    $ fmap (\(out, ref) -> (ref, out))
                    $ txOutRefs tx
  | otherwise = mempty

fromOnChainTx :: OnChainTx -> ChainIndexTx
fromOnChainTx = \case
    Valid tx@Tx{txInputs, txOutputs, txValidRange, txData, txMintScripts} ->
        let (validatorHashes, otherDataHashes, redeemers) = validators txInputs in
        ChainIndexTx
            { _citxTxId = txId tx
            , _citxInputs = txInputs
            , _citxOutputs = txOutputs
            , _citxValidRange = txValidRange
            , _citxData = txData <> otherDataHashes
            , _citxRedeemers = redeemers
            , _citxMintingPolicies = mintingPolicies txMintScripts
            , _citxStakeValidators = mempty
            , _citxValidators = validatorHashes
            , _citxIsValid = True
            }
    Invalid tx@Tx{txCollateral, txValidRange, txData, txInputs, txMintScripts} ->
        let (validatorHashes, otherDataHashes, redeemers) = validators txInputs in
        ChainIndexTx
            { _citxTxId = txId tx
            , _citxInputs = txCollateral
            , _citxOutputs = mempty
            , _citxValidRange = txValidRange
            , _citxData = txData <> otherDataHashes
            , _citxRedeemers = redeemers
            , _citxMintingPolicies = mintingPolicies txMintScripts
            , _citxStakeValidators = mempty
            , _citxValidators = validatorHashes
            , _citxIsValid = False
            }

mintingPolicies :: Set MintingPolicy -> Map MintingPolicyHash MintingPolicy
mintingPolicies =
    let withHash mps = (mintingPolicyHash mps, mps) in
    Map.fromList . fmap withHash . Set.toList

validators :: Set TxIn -> (Map ValidatorHash Validator, Map DatumHash Datum, Map RedeemerHash Redeemer)
validators =
    let withHash (ConsumeScriptAddress val red dat) =
            ( Map.singleton (validatorHash val) val
            , Map.singleton (datumHash dat) dat
            , Map.singleton (redeemerHash red) red
            )
        withHash ConsumePublicKeyAddress    = mempty
    in foldMap (maybe mempty withHash . txInType) . Set.toList
