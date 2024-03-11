---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:10.211081-07:00
description: "YAML, \u091C\u093F\u0938\u0915\u093E \u092A\u0942\u0930\u093E \u0928\
  \u093E\u092E \"YAML Ain't Markup Language\" \u0939\u0948, \u090F\u0915 \u092E\u093E\
  \u0928\u0935-\u0905\u0928\u0941\u0915\u0942\u0932 \u0921\u0947\u091F\u093E \u0938\
  \u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C\u0947\u0936\u0928 \u092E\u093E\u0928\
  \u0915 \u0939\u0948 \u091C\u093F\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0938\u092D\u0940 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\
  \u0902\u0917 \u092D\u093E\u0937\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964\
  \u2026"
lastmod: '2024-03-11T00:14:26.358128-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u091C\u093F\u0938\u0915\u093E \u092A\u0942\u0930\u093E \u0928\u093E\
  \u092E \"YAML Ain't Markup Language\" \u0939\u0948, \u090F\u0915 \u092E\u093E\u0928\
  \u0935-\u0905\u0928\u0941\u0915\u0942\u0932 \u0921\u0947\u091F\u093E \u0938\u0940\
  \u0930\u093F\u092F\u0932\u093E\u0907\u091C\u0947\u0936\u0928 \u092E\u093E\u0928\u0915\
  \ \u0939\u0948 \u091C\u093F\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0938\
  \u092D\u0940 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092D\u093E\u0937\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\
  \u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964\u2026"
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
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
