data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq)

type CountryCode = Integer
type PhoneNo = Integer

data Phone = Phone { phoneType :: PhoneType 
                    , countryCode :: CountryCode 
                    , phoneNo :: PhoneNo
                    } deriving (Show, Eq)


countryCodeError = "Negative country code"
phoneNoError = "Negative phone number"

makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone phoneType code num
    | code < 0 = error countryCodeError
    | num < 0 = error phoneNoError
    | otherwise = Phone phoneType code num 


