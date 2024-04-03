---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:43.003759-07:00
description: "\u0915\u0948\u0938\u0947: Haskell \u092E\u0947\u0902, CSV \u092B\u093E\
  \u0907\u0932\u094B\u0902 \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0928\u0947\
  \ \u0915\u093E \u0915\u093E\u092E `cassava` \u092A\u0941\u0938\u094D\u0924\u0915\
  \u093E\u0932\u092F \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\
  , \u091C\u094B \u0907\u0938 \u0909\u0926\u094D\u0926\u0947\u0936\u094D\u092F \u0915\
  \u0947 \u0932\u093F\u090F \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0924\
  \u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\u094D\u0924\
  \u0915\u093E\u0932\u092F\u094B\u0902\u2026"
lastmod: '2024-03-13T22:44:52.443130-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u092E\u0947\u0902, CSV \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\
  \u094B \u0938\u0902\u092D\u093E\u0932\u0928\u0947 \u0915\u093E \u0915\u093E\u092E\
  \ `cassava` \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0915\u093F\u092F\u093E\
  \ \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948, \u091C\u094B \u0907\u0938\
  \ \u0909\u0926\u094D\u0926\u0947\u0936\u094D\u092F \u0915\u0947 \u0932\u093F\u090F\
  \ \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0924\u0943\u0924\u0940\u092F\
  -\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\
  \u094B\u0902 \u092E\u0947\u0902 \u0938\u0947 \u090F\u0915 \u0939\u0948\u0964 \u0928\
  \u0940\u091A\u0947 `cassava` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0915\u0947 CSV \u092B\u093E\u0907\u0932\u094B\u0902 \u0938\u0947 \u092A\u0922\u093C\
  \u0928\u0947 \u0914\u0930 \u0909\u0928\u092E\u0947\u0902 \u0932\u093F\u0916\u0928\
  \u0947 \u0915\u0947 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u090F \u0917\
  \u090F \u0939\u0948\u0902\u0964\n\n**1."
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 37
---

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
