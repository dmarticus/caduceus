{-# LANGUAGE OverloadedStrings #-}
module IBANRegistryExamples
  (examples) where

import Wires.IBAN.Internal
import Data.Text (Text)

examples :: [Text]
examples = [ "AL47212110090000000235698741"
           , "AL47 2121 1009 0000 0002 3569 8741"
           , "AD1200012030200359100100"
           , "AD12 0001 2030 2003 5910 0100"
           , "AT611904300234573201"
           , "AT61 1904 3002 3457 3201"
           , "AZ21NABZ00000000137010001944"
           , "AZ21 NABZ 0000 0000 1370 1000 1944"
           , "BH67BMAG00001299123456"
           , "BH67 BMAG 0000 1299 1234 56"
           , "BE68539007547034"
           , "BE68 5390 0754 7034"
           , "BA391290079401028494"
           , "BA39 1290 0794 0102 8494"
           , "BR9700360305000010009795493P1"
           , "BR1800000000141455123924100C2"
           , "BR97 0036 0305 0000 1000 9795 493P 1"
           , "BR18 0000 0000 1414 5512 3924 100C 2"
           , "BG80BNBG96611020345678"
           , "BG80 BNBG 9661 1020 3456 78"
           , "CR0515202001026284066"
           , "CR05 1520 2001 0262 8406 6"
           , "HR1210010051863000160"
           , "HR12 1001 0051 8630 0016 0"
           , "CY17002001280000001200527600"
           , "CY17 0020 0128 0000 0012 0052 7600"
           , "CZ6508000000192000145399"
           , "CZ9455000000001011038930"
           , "CZ65 0800 0000 1920 0014 5399"
           , "CZ94 5500 0000 0010 1103 8930"
           , "DK5000400440116243"
           , "FO6264600001631634"
           , "GL8964710001000206"
           , "DK50 0040 0440 1162 43"
           , "FO62 6460 0001 6316 34"
           , "GL89 6471 0001 0002 06"
           , "DO28BAGR00000001212453611324"
           , "DO28 BAGR 0000 0001 2124 5361 1324"
           , "EE382200221020145685"
           , "EE38 2200 2210 2014 5685"
           , "FI2112345600000785"
           , "FI5542345670000081"
           , "FI21 1234 5600 0007 85"
           , "FR1420041010050500013M02606"
           , "FR14 2004 1010 0505 0001 3M02 606"
           , "GE29NB0000000101904917"
           , "GE29 NB00 0000 0101 9049 17"
           , "DE89370400440532013000"
           , "DE89 3704 0044 0532 0130 00"
           , "GI75NWBK000000007099453"
           , "GI75 NWBK 0000 0000 7099 453"
           , "GR1601101250000000012300695"
           , "GR16 0110 1250 0000 0001 2300 695"
           , "GT82TRAJ01020000001210029690"
           , "GT82 TRAJ 0102 0000 0012 1002 9690"
           , "HU42117730161111101800000000"
           , "HU42 1177 3016 1111 1018 0000 0000"
           , "IS140159260076545510730339"
           , "IS14 0159 2600 7654 5510 7303 39"
           , "IE29AIBK93115212345678"
           , "IE29 AIBK 9311 5212 3456 78"
           , "IL620108000000099999999"
           , "IL62 0108 0000 0009 9999 999"
           , "IT60X0542811101000000123456"
           , "IT60 X054 2811 1010 0000 0123 456"
           , "JO94CBJO0010000000000131000302"
           , "JO94 CBJO 0010 0000 0000 0131 0003 02"
           , "KZ86125KZT5004100100"
           , "KZ86 125K ZT50 0410 0100"
           , "KW81CBKU0000000000001234560101"
           , "KW81 CBKU 0000 0000 0000 1234 5601 01"
           , "LV80BANK0000435195001"
           , "LV80 BANK 0000 4351 9500 1"
           , "LB62099900000001001901229114"
           , "LB62 0999 0000 0001 0019 0122 9114"
           , "LI21088100002324013AA"
           , "LI21 0881 0000 2324 013A A"
           , "LT121000011101001000"
           , "LT12 1000 0111 0100 1000"
           , "LU280019400644750000"
           , "LU28 0019 4006 4475 0000"
           , "MK07250120000058984"
           , "MK07 2501 2000 0058 984"
           , "MT84MALT011000012345MTLCAST001S"
           , "MT84 MALT 0110 0001 2345 MTLC AST0 01S"
           , "MR1300020001010000123456753"
           , "MR13 0002 0001 0100 0012 3456 753"
           , "MU17BOMM0101101030300200000MUR"
           , "MU17 BOMM 0101 1010 3030 0200 000M UR"
           , "MD24AG000225100013104168"
           , "MD24 AG00 0225 1000 1310 4168"
           , "MC58 11222 00001 0123456789030"
           , "MC58 11222 00001 0123456789030"
           , "ME25505000012345678951"
           , "ME25 5050 0001 2345 6789 51"
           , "NL91ABNA0417164300"
           , "NL91 ABNA 0417 1643 00"
           , "NO9386011117947"
           , "NO93 8601 1117 947"
           , "PK36SCBL0000001123456702"
           , "PK36 SCBL 0000 0011 2345 6702"
           , "PS92PALS000000000400123456702"
           , "PS92 PALS 0000 0000 0400 1234 5670 2"
           , "PL61109010140000071219812874"
           , "PL61 1090 1014 0000 0712 1981 2874"
           , "PT50000201231234567890154"
           , "PT50 0002 0123 1234 5678 9015 4"
           , "RO49AAAA1B31007593840000"
           , "RO49 AAAA 1B31 0075 9384 0000"
           , "QA58DOHB00001234567890ABCDEFG"
           , "QA58 DOHB 0000 1234 5678 90AB CDEF G"
           , "SM86U0322509800000000270100"
           , "SM86 U032 2509 8000 0000 0270 100"
           , "SA0380000000608010167519"
           , "SA03 8000 0000 6080 1016 7519"
           , "RS35260005601001611379"
           , "RS35 2600 0560 1001 6113 79"
           , "SK3112000000198742637541"
           , "SK31 1200 0000 1987 4263 7541"
           , "SI56263300012039086"
           , "SI56 2633 0001 2039 086"
           , "ES9121000418450200051332"
           , "ES91 2100 0418 4502 0005 1332"
           , "SE4550000000058398257466"
           , "SE45 5000 0000 0583 9825 7466"
           , "CH9300762011623852957"
           , "CH93 0076 2011 6238 5295 7"
           , "TN5910006035183598478831"
           , "TN59 1000 6035 1835 9847 8831"
           , "TR330006100519786457841326"
           , "TR33 0006 1005 1978 6457 8413 26"
           , "AE070331234567890123456"
           , "AE07 0331 2345 6789 0123 456"
           , "GB29NWBK60161331926819"
           , "GB29 NWBK 6016 1331 9268 19"
           , "VG96VPVG0000012345678901"
           , "VG96 VPVG 0000 0123 4567 8901"
           ]
