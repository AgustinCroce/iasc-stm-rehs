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
   setTransaction,
   clearTransaction,
   clearAttributeTransaction,
   setSchemaTransaction,
   reverseTransaction,
   upcaseTransaction) where

import Control.Concurrent.STM
import Data.Map(Map, empty, (!), insert, fromList)
import Data.Char(toUpper)

type Table = TVar (Map String String)
type SlotTransaction = Table -> STM (String)

newTable :: STM Table
newTable = newTvar empty

setTransaction :: String -> String -> SlotTransaction
setTransaction attribute value = \table -> do 
  modifyTVar table (\map -> insert attribute value map)
  return value

clearAttributeTransaction :: String -> SlotTransaction
clearAttributeTransaction = flip setTransaction ""

clearTransaction :: SlotTransaction
clearTransaction  = \table -> do 
  writeTVar table empty
  return "<OK>"

applyToAttribute :: (String -> String) -> String -> Table -> STM (String)
applyToAttribute f attribute table = do 
  map <- readTVar table
  return $ f $ map ! attribute 

readTransaction :: String -> SlotTransaction
readTransaction = applyToAttribute id

reverseTransaction :: String -> SlotTransaction
reverseTransaction = applyToAttribute reverse

upcaseTransaction :: String -> SlotTransaction
upcaseTransaction = applyToAttribute $ map toUpper

setSchemaTransaction :: [String] -> SlotTransaction
setSchemaTransaction schema = \table -> do
  writeTVar table $ fromList([(attribute, "") | attribute <- schema])
  return "<OK>"