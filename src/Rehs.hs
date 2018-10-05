------------------------------------------------------------
-- Main definitions.
-- This modules defines a Table and its operations
-- Notice that all functions in this module return STM's
------------------------------------------------------------

module Rehs (
   Table,
   SlotTransaction,
   newTable,
   readTransaction,
   setSchemaTransaction) where

import Control.Concurrent.STM
import Data.Map(Map, empty, (!), insert, fromList, lookup)
import Data.Char(toUpper)

type Table = Map String (TVar String)
type SlotTransaction = Table -> STM (String)

newTable :: STM Table
newTable = return empty

-- setTransaction :: String -> String -> SlotTransaction
-- setTransaction attribute value = \table -> do 
--   modifyTVar table (\map -> insert attribute value map)
--   return value

-- clearAttributeTransaction :: String -> SlotTransaction
-- clearAttributeTransaction = flip setTransaction ""

-- clearTransaction :: SlotTransaction
-- clearTransaction  = \table -> do 
--   writeTVar table empty
--   return "<OK>"

readSTMMaybe :: Maybe(TVar String) -> STM(String)
readSTMMaybe (Just var) = readTVar var
readSTMMaybe Nothing = return "<404>" 

safeGet :: String -> Table -> STM(String)
safeGet attribute table = readSTMMaybe $ Data.Map.lookup attribute table


applyToAttribute :: (String -> String) -> String -> Table -> STM (String)
applyToAttribute f attribute table = do 
  value <- safeGet attribute table
  return $ f value

readTransaction :: String -> SlotTransaction
readTransaction = applyToAttribute id

-- reverseTransaction :: String -> SlotTransaction
-- reverseTransaction = applyToAttribute reverse

-- upcaseTransaction :: String -> SlotTransaction
-- upcaseTransaction = applyToAttribute $ map toUpper

setSchemaTransaction :: [String] -> SlotTransaction
setSchemaTransaction schema = \table -> do
  foldl (\tableSTM attribute -> do
    table <- tableSTM
    element <- newTVar ""
    return $ insert attribute element table) (return table) schema
  return "<OK>"