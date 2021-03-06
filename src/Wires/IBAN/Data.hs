{-# LANGUAGE OverloadedStrings #-}
module Wires.IBAN.Data
  (patterns) where

import Data.Text (Text)

patterns :: [Text]
patterns = [ "AL2!n8!n16!c"
          , "AD2!n4!n4!n12!c"
          , "AT2!n5!n11!n"
          , "AZ2!n4!a20!c"
          , "BH2!n4!a14!c"
          , "BE2!n3!n7!n2!n"
          , "BA2!n3!n3!n8!n2!n"
          , "BR2!n8!n5!n10!n1!a1!c"
          , "BG2!n4!a4!n2!n8!c"
          , "CR2!n3!n14!n"
          , "HR2!n7!n10!n"
          , "CY2!n3!n5!n16!c"
          , "CZ2!n4!n6!n10!n"
          , "FI2!n6!n7!n1!n"
          , "FR2!n5!n5!n11!c2!n"
          , "GE2!n2!a16!n"
          , "DE2!n8!n10!n"
          , "GI2!n4!a15!c"
          , "GR2!n3!n4!n16!c"
          , "GT2!n4!c20!c"
          , "HU2!n3!n4!n1!n15!n1!n"
          , "IS2!n4!n2!n6!n10!n"
          , "IE2!n4!a6!n8!n"
          , "IL2!n3!n3!n13!n"
          , "IT2!n1!a5!n5!n12!c"
          , "KW2!n4!a22!n"
          , "LV2!n4!a13!c"
          , "LB2!n4!n20!c"
          , "LI2!n5!n12!c"
          , "LT2!n5!n11!n"
          , "LU2!n3!n13!c"
          , "MK2!n3!n10!c2!n"
          , "MT2!n4!a5!n18!c"
          , "MR135!n5!n11!n2!n"
          , "MU2!n4!a2!n2!n12!n3!n3!a"
          , "MD2!n20!c"
          , "MC2!n5!n5!n11!c2!n"
          , "ME2!n3!n13!n2!n"
          , "NL2!n4!a10!n"
          , "NO2!n4!n6!n1!n"
          , "PK2!n4!a16!c"
          , "PS2!n4!a21!c"
          , "PL2!n8!n16n"
          , "PT2!n4!n4!n11!n2!n"
          , "RO2!n4!a16!c"
          , "QA2!n4!a21!c"
          , "SM2!n1!a5!n5!n12!c"
          , "SA2!n2!n18!c"
          , "RS2!n3!n13!n2!n"
          , "SK2!n4!n6!n10!n"
          , "SI2!n5!n8!n2!n"
          , "ES2!n4!n4!n1!n1!n10!n"
          , "SE2!n3!n16!n1!n"
          , "CH2!n5!n12!c"
          , "TN592!n3!n13!n2!n"
          , "TR2!n5!n1!c16!c"
          , "AE2!n3!n16!n"
          , "GB2!n4!a6!n8!n"
          , "VG2!n4!a16!n"
          , "DK2!n4!n9!n1!n"
          , "FO2!n4!n9!n1!n"
          , "GL2!n4!n9!n1!n"
          , "DO2!n4!c20!n"
          , "EE2!n2!n2!n11!n1!n"
          , "KZ2!n3!n13!c"
          , "JO4!a4!n18!c" ]
