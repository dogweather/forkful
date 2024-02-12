---
title:                "CSV के साथ काम करना"
aliases: - /hi/haskell/working-with-csv.md
date:                  2024-02-03T19:20:43.003759-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

CSVs (Comma-Separated Values) के साथ काम करना तालिका-आधारित डेटा को एक सरल, पाठ-आधारित प्रारूप में संग्रहीत करने वाली फाइलों को पार्सिंग और उत्पन्न करने की प्रक्रिया होती है। प्रोग्रामर अक्सर इस कार्य में संलग्न होते हैं ताकि वे स्प्रेडशीट्स, डेटाबेस से डेटा को आयात या निर्यात करने में कुशलता से या विभिन्न कार्यक्रमों के बीच डेटा आदान-प्रदान की सुविधा प्रदान कर सकें।

## कैसे:

Haskell में, CSV फाइलों को संभालने का काम `cassava` पुस्तकालय का उपयोग करके किया जा सकता है, जो इस उद्देश्य के लिए लोकप्रिय तृतीय-पक्ष पुस्तकालयों में से एक है। नीचे `cassava` का उपयोग करके CSV फाइलों से पढ़ने और उनमें लिखने के उदाहरण दिए गए हैं।

**1. एक CSV फाइल को पढ़ना:**

पहले, सुनिश्चित करें कि आपके पास `cassava` स्थापित है, इसे अपनी परियोजना की कैबल फाइल में जोड़कर या स्टैक का उपयोग करके।

यहाँ एक साधारण उदाहरण है जो एक CSV फाइल को पढ़ता है और प्रत्येक रिकॉर्ड को प्रिंट करता है। हम मानते हैं कि CSV फाइल में दो स्तंभ हैं: नाम और उम्र।

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " is " ++ show (age :: Int) ++ " वर्ष का है।"
```

मान लें `people.csv` में शामिल है:
```
John,30
Jane,25
```
आउटपुट होगा:
```
John 30 वर्ष का है।
Jane 25 वर्ष की है।
```

**2. एक CSV फाइल लिखना:**

एक CSV फाइल बनाने के लिए, आप `cassava` से `encode` फ़ंक्शन का उपयोग कर सकते हैं।

यहाँ कैसे आप रिकॉर्ड की एक सूची को एक CSV फाइल में लिख सकते हैं:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

इस प्रोग्राम को चलाने के बाद, `output.csv` में शामिल होगा:

```
John,30
Jane,25
```

Haskell में `cassava` पुस्तकालय का उपयोग करके CSV फाइलों के साथ काम करने का यह संक्षिप्त परिचय दोनों कैसे पढ़ने और लिखने का तरीका दर्शाता है, जिससे डेटा हेरफेर कार्य उन लोगों के लिए अधिक सुलभ हो जाते हैं जो भाषा के लिए नए हैं।
