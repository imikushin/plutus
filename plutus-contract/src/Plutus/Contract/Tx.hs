{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeApplications   #-}
module Plutus.Contract.Tx where

import           Control.Lens
import           Data.Maybe                       (fromMaybe)

import           Data.Map                         (Map)
import           Ledger                           (Redeemer (..), TxOutRef, TxOutTx, Validator)
import qualified Ledger.Address                   as Address
import           Ledger.AddressMap                (AddressMap)
import           Ledger.Constraints.TxConstraints (UntypedConstraints)
import           Ledger.Tx                        (ChainIndexTxOut)
import qualified Plutus.Contract.Typed.Tx         as Typed
import qualified PlutusTx

-- | A set of constraints for a transaction that collects script outputs
--   from the address of the given validator script, using the same redeemer
--   script for all outputs.
collectFromScript
    :: Map Address.Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScript = collectFromScriptFilter (\_ -> const True)

-- | A set of constraints for a transaction that collects script outputs
--   from the address of the given validator script, using the same redeemer
--   script for all outputs.
--
--  TODO Remove uses old chain index
collectFromScriptOld
    :: AddressMap
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScriptOld = collectFromScriptFilterOld (\_ -> const True)

-- | See
--
--  TODO Remove uses old chain index
collectFromScriptFilterOld
    :: (TxOutRef -> TxOutTx -> Bool)
    -> AddressMap
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScriptFilterOld flt am vls (Redeemer red) =
    let mp'  = fromMaybe mempty $ am ^. at (Address.scriptAddress vls)
    in Typed.collectFromScriptFilterOld @PlutusTx.BuiltinData @PlutusTx.BuiltinData flt mp' red

-- | See
collectFromScriptFilter
    :: (TxOutRef -> ChainIndexTxOut -> Bool)
    -> Map Address.Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScriptFilter flt am vls (Redeemer red) =
    let mp'  = fromMaybe mempty $ am ^. at (Address.scriptAddress vls)
    in Typed.collectFromScriptFilter @PlutusTx.BuiltinData @PlutusTx.BuiltinData flt mp' red
