emptyBook :: PhoneBook
emptyBook = []

data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving (Eq, Show)
type PhoneBook = [PhoneBookEntry] 

addEntry :: String -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry "" _ _ _ _ currentbook = currentbook 
addEntry name phonetype ccode phonenum ccodelist [] = 
  PhoneBookEntry { name=name , phone= (readPhone phonetype ccode phonenum ccodelist) } : emptyBook
addEntry name phonetype ccode phonenum ccodelist currentbook = 
  if filteredBook == []
    then PhoneBookEntry { name=name , phone= newPhone } : currentbook
    else if sameNameAndnumber == []
      then PhoneBookEntry { name=name , phone= newPhone } : currentbook
      else currentbook
      where filteredBook = findEntries name currentbook
            newPhone = (readPhone phonetype ccode phonenum ccodelist)
            sameNameAndnumber = filter (\item -> show (phoneNo (phone item)) == ( "PhoneNo " ++ phonenum) ) filteredBook

findEntries :: String -> PhoneBook -> PhoneBook
findEntries "" _ = emptyBook
findEntries _ [] = emptyBook
findEntries nameToFind phonebook = filter (\x -> name x == nameToFind) phonebook

readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone phonetypestr countrycodestr phonenostr ccodelist
  | (code `notElem` ccodelist) == True = error "Unknown country code"
  | otherwise = Phone phoneType countryCode phoneNo
  where codeX = checkCountryCodeStr countrycodestr
        code =  read codeX :: Integer
        phoneType = toPhoneType phonetypestr
        countryCode = toCountryCode code
        phoneNocheck = checkPhoneNoStr phonenostr
        checkedPhoneNoStr = read (checkPhoneNoStr phonenostr) :: Integer 
        phoneNo = toPhoneNo checkedPhoneNoStr

checkPhoneNoStr str 
  | null str == True  = error "Empty phone number"
  | head str == '-'  = error "Negative phone number"
  | otherwise = if all (\x -> x `elem` digitChars) str
                then str
                else error "Incorrect phone number"

checkCountryCodeStr str 
  | null str == True  = error "Empty country code"
  | head str == '+' = checkCountryCodeStr ( tail str)
  | head str == '0' = checkCountryCodeStr ( tail str)
  | otherwise = if all (\x -> x `elem` digitChars) str
                then str
                else error "Incorrect country code"

digitChars = ['0'.. '9']

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Enum)
phoneTypesInList = [ WorkLandline .. Other]

toPhoneType str
  | null str == True    = error "Missing phone type"
  | length (filter (\x -> (show x) == str) phoneTypesInList) <= 0 = error "Incorrect phone type"
  | otherwise = read str :: PhoneType

data CountryCode = CountryCode Integer deriving (Show, Eq)
data PhoneNo = PhoneNo Integer deriving (Show, Eq)

countryCodeError = "Negative country code"
phoneNoError = "Negative phone number"

toCountryCode int 
  | int < 0 = error countryCodeError
  | otherwise = CountryCode int

toPhoneNo int
  | int < 0 = error phoneNoError
  |otherwise = PhoneNo int

data Phone = Phone { phoneType :: PhoneType 
                    , countryCode :: CountryCode 
                    , phoneNo :: PhoneNo
                    } deriving (Show, Eq)

