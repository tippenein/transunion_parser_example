module Signal where

import qualified Data.Map as Map

signalLengthMap :: Map.Map Signal Int
signalLengthMap = Map.fromList [
    (AD02, 127), (AD11, 150), (AH11, 77), (AM01, 22),
    (AO01, 17), (AR01, 60), (AS01, 40), (AT11, 95), (BR01, 338),
    (CA01, 17), (CC11, 47), (CD01, 41), (CH01, 30), (CI01, 19),
    (CL11, 213), (CL13, 230), (CO01, 65), (CORR, 45), (CP11, 80),
    (CR01, 26), (CR02, 22), (CS11, 269), (DA01, 22), (DB01, 59),
    (DC11, 167), (DI01, 33), (DR01, 193), (ED01, 61), (EF01, 61),
    (EM11, 130), (END1, 12), (ERRC, 11), (ERRT, 90), (FA11, 150),
    (FI01, 23), (FT01, 84), (GC01, 54), (ID01, 49), (ID02, 265),
    (IN11, 84), (IN12, 84), (LK11, 54), (MC01, 29), (MI01, 77),
    (ML01, 26), (ML02, 40), (MT01, 171), (NM11, 123), (NU01, 12),
    (OB01, 148), (OB02, 198), (PD01, 100), (PH01, 12), (PH02, 15),
    (PH03, 15), (PH04, 18), (PI11, 25), (PN01, 27), (PN02, 45),
    (PP11, 97), (PR11, 216), (PS01, 127), (PS02, 142), (QH01, 21),
    (RE01, 20), (RR01, 21), (SA11, 106), (SC01, 34), (SD01, 56),
    (SH01, 27), (SH02, 12), (SH03, 10), (SH04, 28), (SH05, 13),
    (SH06, 14), (SM11, 44), (SV01, 67), (SV02, 31), (SV03, 47),
    (TA01, 12), (TC01, 30), (TM11, 46), (TR11, 332), (TR12, 122),
    (TR13, 387), (TT01, 33), (TU4E, 62), (TU4R, 62), (TX01, 172),
    (UA11, 130), (UF11, 130), (VH01, 184), (VS01, 33), (WS01, 10),
    (YI01, 36), (ZC01, 22)]

data Signal
  = AD02 | AD11 | AH11 | AM01 |
    AO01 | AR01 | AS01 | AT11 | BR01 |
    CA01 | CC11 | CD01 | CH01 | CI01 |
    CL11 | CL13 | CO01 | CORR | CP11 |
    CR01 | CR02 | CS11 | DA01 | DB01 |
    DC11 | DI01 | DR01 | ED01 | EF01 |
    EM11 | END1 | ERRC | ERRT | FA11 |
    FI01 | FT01 | GC01 | ID01 | ID02 |
    IN11 | IN12 | LK11 | MC01 | MI01 |
    ML01 | ML02 | MT01 | NM11 | NU01 |
    OB01 | OB02 | PD01 | PH01 | PH02 |
    PH03 | PH04 | PI11 | PN01 | PN02 |
    PP11 | PR11 | PS01 | PS02 | QH01 |
    RE01 | RR01 | SA11 | SC01 | SD01 |
    SH01 | SH02 | SH03 | SH04 | SH05 |
    SH06 | SM11 | SV01 | SV02 | SV03 |
    TA01 | TC01 | TM11 | TR11 | TR12 |
    TR13 | TT01 | TU4E | TU4R | TX01 |
    UA11 | UF11 | VH01 | VS01 | WS01 |
    YI01 | ZC01
  deriving (Show, Enum, Ord, Eq, Read)


allSignals :: [String]
allSignals = map show [AD02 ..]

--array = ["TU4R062311                        1201F 0273814620150824124331"]
--{ "TU4R": [
--    {
--      "segment_type": "TU4R",
--      "segment_length": "062",
--      "version_switch": "3",
--      "country_code": "1",
--      "language_indicator": "1",
--      "user_reference_number": "                        ",
--      "bureau_market": "12",
--      "bureau_submarket": "01",
--      "industry_code": "F ",
--      "inquiring_subscriber_code": "02738146",
--      "transaction_date": "20150824",
--      "transaction_time": "124331"
--    }
--  ] }
data Transunion4R
  = Transunion4R
  { segmentType             :: String
  , segmentLength           :: String
  , versionSwitch           :: Int
  , countryCode             :: Int
  , languageIndicator       :: Int
  , userReferenceNumber     :: Int
  , bureau_market           :: Int
  , bureau_submarket        :: String
  , industryCode            :: String
  , inquiringSubscriberCode :: String
  , transactionDate         :: Int
  , transactionTime         :: Int
  } deriving (Show)

