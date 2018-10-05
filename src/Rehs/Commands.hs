------------------------------------------------------------
-- Command parsing functions
-- This module declares functions for parsing the user input
-- and converting it into Slot Transaction - functions that
-- return an STM ().
------------------------------------------------------------

module Rehs.Commands (
    parseSlotTransactionLine) where

import Data.List.Split (splitOn)
import Rehs (SlotTransaction, 
  setSchemaTransaction,
  readTransaction)

type Command = [String]

parseSlotTransactionLine :: String -> SlotTransaction
parseSlotTransactionLine = parseSlotTransactionCommand . splitOn ":"

parseSlotTransactionCommand :: Command -> SlotTransaction
-- parseSlotTransactionCommand ["upcase", attribute] = Rehs.upcaseTransaction attribute
-- parseSlotTransactionCommand ["reverse", attribute] = Rehs.reverseTransaction attribute
-- parseSlotTransactionCommand ["clear", attribute] = Rehs.clearAttributeTransaction attribute
-- parseSlotTransactionCommand ["set", attribute, value] = Rehs.setTransaction attribute value
-- parseSlotTransactionCommand ["clear_all"] = Rehs.clearTransaction
parseSlotTransactionCommand ["read", attribute] = Rehs.readTransaction attribute
parseSlotTransactionCommand ("schema":schema) = Rehs.setSchemaTransaction schema