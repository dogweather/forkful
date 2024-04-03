---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:51.839516-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Haskell \u092E\u0947\
  \u0902, \u0906\u092A \u0915\u093F\u0938\u0940 \u0936\u092C\u094D\u0926 \u0915\u094B\
  \ Capitalize \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092E\u093E\
  \u0928\u0915 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\
  \u0902, \u092C\u093F\u0928\u093E \u0915\u093F\u0938\u0940 \u0924\u0943\u0924\u0940\
  \u092F-\u092A\u0915\u094D\u0937 \u0915\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E\
  \ \u0915\u0947\u0964."
lastmod: '2024-03-13T22:44:52.375519-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u092E\u0947\u0902, \u0906\u092A \u0915\u093F\u0938\u0940 \u0936\
  \u092C\u094D\u0926 \u0915\u094B Capitalize \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u092E\u093E\u0928\u0915 \u092A\u0941\u0938\u094D\u0924\u0915\
  \u093E\u0932\u092F \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902, \u092C\u093F\u0928\u093E \u0915\u093F\u0938\
  \u0940 \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u0915\u0940 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u0940 \u0906\u0935\u0936\
  \u094D\u092F\u0915\u0924\u093E \u0915\u0947\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u093E"
weight: 2
---

## कैसे करें:
Haskell में, आप किसी शब्द को Capitalize करने के लिए मानक पुस्तकालय का उपयोग कर सकते हैं, बिना किसी तृतीय-पक्ष की लाइब्रेरी की आवश्यकता के।

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- नमूना उपयोग:
main = putStrLn $ capitalize "hello world"
```

आउटपुट:
```
Hello world
```

अधिक जटिल परिस्थितियों या उपयोग की आसानी के लिए, आप `text` जैसी तृतीय-पक्ष की लाइब्रेरी का उपयोग करना चाहेंगे, जो Haskell में कुशल स्ट्रिंग संचालन के लिए लोकप्रिय है।

पहले, आपको अपनी परियोजना की निर्भरताओं में `text` जोड़ने की आवश्यकता है। फिर, आप इसके फंक्शन्स का उपयोग करके निम्नलिखित तरीके से एक स्ट्रिंग को Capitalize कर सकते हैं:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- टेक्स्ट लाइब्रेरी के साथ नमूना उपयोग:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

आउटपुट:
```
Hello world
```

ये दोनों उदाहरण Haskell में, बिना या तृतीय-पक्ष की लाइब्रेरियों के साथ, स्ट्रिंग को Capitalize करने के आसान और प्रभावी तरीके प्रदर्शित करते हैं।
