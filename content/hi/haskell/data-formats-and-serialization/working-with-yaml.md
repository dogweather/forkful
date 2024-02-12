---
title:                "YAML के साथ काम करना"
date:                  2024-02-03T19:26:10.211081-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML, जिसका पूरा नाम "YAML Ain't Markup Language" है, एक मानव-अनुकूल डेटा सीरियलाइजेशन मानक है जिसका उपयोग सभी प्रोग्रामिंग भाषाओं के लिए किया जा सकता है। प्रोग्रामर अक्सर YAML का उपयोग कॉन्फ़िगरेशन फ़ाइलों और भाषाओं के बीच डेटा विनिमय के लिए करते हैं क्योंकि इसकी पढ़ने योग्यता और सरल संरचना होती है।

## कैसे:

Haskell में YAML प्रोसेसिंग के लिए बिल्ट-इन सपोर्ट नहीं है, लेकिन आप `yaml` और `aeson` जैसी तृतीय-पक्ष लाइब्रेरी का उपयोग करके YAML डेटा को पार्स और जेनरेट कर सकते हैं। यहाँ पर आपको शुरू करने का तरीका दिया गया है:

### YAML पढ़ना
सबसे पहले, अपने प्रोजेक्ट की निर्भरताओं में `yaml` पैकेज को जोड़ें। फिर, आप एक साधारण YAML दस्तावेज़ को पार्स करने के लिए निम्नलिखित उदाहरण का उपयोग कर सकते हैं:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- उदाहरण YAML डेटा
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- YAML दस्तावेज़ से मेल खाने वाली एक डेटा संरचना परिभाषित करें
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "YAML पार्स करने में त्रुटि: " ++ show err
    Right person -> print person
```
ऊपर दिए गए कोड के लिए नमूना आउटपुट ऐसा दिख सकता है:
```
Person {name = "John Doe", age = 30}
```

### YAML लिखना
Haskell डेटा संरचनाओं से YAML जेनरेट करने के लिए, आप नीचे दिखाए अनुसार `yaml` पैकेज की एन्कोडिंग कार्यक्षमताओं का उपयोग कर सकते हैं:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- पिछले उदाहरण से Person डेटा संरचना का उपयोग करते हुए

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
इस प्रोग्राम का आउटपुट एक YAML-फॉरमेटेड स्ट्रिंग होगा:
```
name: Jane Doe
age: 25
```

ये उदाहरण Haskell में YAML के साथ काम करने के लिए एक प्रारंभिक बिंदु के रूप में काम करना चाहिए। अपनी आवश्यकताओं के आधार पर, आप इन लाइब्रेरियों द्वारा प्रदान की गई अधिक उन्नत सुविधाओं और विकल्पों का पता लगाना चाह सकते हैं।
