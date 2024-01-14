---
title:                "Haskell: csv के साथ काम करना"
simple_title:         "csv के साथ काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

CSV (Comma Separated Values) फॉर्मेट डेटा को स्टोर और एक्सट्रैक्ट करने के लिए सबसे उपयुक्त तरीके में से एक है। हास्केल में CSV का उपयोग करके आप अपने डेटा को आसानी से संग्रहित कर सकते हैं और उसे विभिन्न विश्लेषण टूल्स में इम्पोर्ट कर सकते हैं।

## कैसे करें

CSV पार्सिंग के लिए, हास्केल में `Data.Csv` मॉड्यूल का उपयोग किया जाता है। निम्न उदाहरण में, हम एक CSV फाइल को पार्स करके उसका डेटा एक सूची में रखते हैं।

```Haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

data Person = Person
  { name :: !String
  , age :: !Int
  } deriving (Generic, Show)

-- दिए गए फ़ाइल पथ से CSV फ़ाइल को लोड करें
main :: IO ()
main = do
  csvData <- BL.readFile "persons.csv"
  let decodedCsv = decode NoHeader csvData :: Either String (V.Vector Person)
  case decodedCsv of
    Left err -> putStrLn err
    Right v -> V.mapM_ print v -- Person डेटा को सूची के रूप में प्रिंट करें
```

आप अपनी प्रोजेक्ट में इससे भिन्न तरीकों से भी CSV का उपयोग कर सकते हैं, लेकिन यह उदाहरण आपको हास्केल में CSV फाइल्स के साथ काम करने के लिए एक आरंभिक धारणा देगा।

## गहराई में जाएं

CSV पार्सिंग के लिए, हास्केल में कई प्राकृतिक बनावटों का उपयोग किया जाता है, जो कई फायदे प्रदान करते हैं। एक उदाहरण के रूप में `CSV.Parser` मॉड्यूल वेरीजन 0.1.0.0 परिभाषा के साथ `DecodeOptions` को अपडेट करने की अनुमति देता है, जो आपको CSV फ़ाइल की संरचना और डेटा टाइप के साथ खेलने में अधिक