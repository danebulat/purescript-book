module Test.MySolutions where

import Prelude

import Data.AddressBook
import Data.List  (head, filter, nubByEq)
import Data.Maybe (isJust, Maybe)

-- Return entry based on street name
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterStreet
  where 
    filterStreet :: Entry -> Boolean
    filterStreet entry = entry.address.street == street

-- Check if name appears in an address book
isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = isJust <<< findEntry firstName lastName 

-- Remove duplicate address book entries
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubByEq pred book
  where 
    pred :: Entry -> Entry -> Boolean
    pred e1 e2 = e1.firstName == e2.firstName &&
                 e1.lastName  == e2.lastName

