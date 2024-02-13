module Phone_book_map
  ( PhoneBook(..),
    Name,
    emptyBook,
    findEntries,
    addEntry
  ) where

import Phone_type2

import qualified Data.Map as Map
type Name = String
type PhoneBook = Map.Map Name [Phone]

emptyBook :: PhoneBook
emptyBook = Map.empty

addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry "" _ _ _ _ currentbook = currentbook 
addEntry name phonetype ccode phonenum ccodelist book
  | Map.null book = Map.insert name [newPhone] emptyBook
  | foundPhones == [] = Map.insert name [newPhone] book
  | sameNameAndnumber == [] = Map.insertWith (++) name [newPhone] book -- add new phone to existing name
  | otherwise = book
  where newPhone = (readPhone phonetype ccode phonenum ccodelist)
        foundPhones = findEntries name book -- returns list of Phones
        sameNameAndnumber = filter (\item -> show (phoneNo item) == ( "PhoneNo " ++ phonenum)) foundPhones

-- Find a list of Phones by a name
findEntries :: Name -> PhoneBook -> [Phone]
findEntries "" _ = []
findEntries nameToFind phonebook
  | Map.null phonebook = []  -- if map is empty, return empty list
findEntries nameToFind phonebook
  | Map.null checkMap = []  -- if map is empty, return empty list
  | otherwise =  phonesInList (Map.lookup nameToFind checkMap)
  where checkMap = Map.filterWithKey (\k _ -> k == nameToFind) phonebook

phonesInList :: Maybe [Phone] -> [Phone]
phonesInList Nothing = []
phonesInList (Just a) = a
