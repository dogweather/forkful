---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:51.839516-07:00
description: "\u0915\u093F\u0938\u0940 \u0936\u092C\u094D\u0926 \u0915\u094B Capitalize\
  \ \u0915\u0930\u0928\u0947 \u0915\u093E \u0905\u0930\u094D\u0925 \u0939\u0948, \u0926\
  \u093F\u090F \u0917\u090F \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\
  \u0947 \u092A\u0939\u0932\u0947 \u0905\u0915\u094D\u0937\u0930 \u0915\u094B \u092C\
  \u0921\u093C\u0947 \u0905\u0915\u094D\u0937\u0930 (uppercase) \u092E\u0947\u0902\
  \ \u092C\u0926\u0932\u0928\u093E, \u0938\u093E\u0925 \u0939\u0940 \u092C\u093E\u0915\
  \u0940 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u091B\u094B\u091F\
  \u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902\u2026"
lastmod: '2024-03-11T00:14:26.288461-06:00'
model: gpt-4-0125-preview
summary: "\u0915\u093F\u0938\u0940 \u0936\u092C\u094D\u0926 \u0915\u094B Capitalize\
  \ \u0915\u0930\u0928\u0947 \u0915\u093E \u0905\u0930\u094D\u0925 \u0939\u0948, \u0926\
  \u093F\u090F \u0917\u090F \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\
  \u0947 \u092A\u0939\u0932\u0947 \u0905\u0915\u094D\u0937\u0930 \u0915\u094B \u092C\
  \u0921\u093C\u0947 \u0905\u0915\u094D\u0937\u0930 (uppercase) \u092E\u0947\u0902\
  \ \u092C\u0926\u0932\u0928\u093E, \u0938\u093E\u0925 \u0939\u0940 \u092C\u093E\u0915\
  \u0940 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u091B\u094B\u091F\
  \u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
किसी शब्द को Capitalize करने का अर्थ है, दिए गए स्ट्रिंग के पहले अक्षर को बड़े अक्षर (uppercase) में बदलना, साथ ही बाकी अक्षरों को छोटे अक्षरों (lowercase) में रखना। प्रोग्रामर इस काम को आउटपुट की फॉर्मेटिंग, टेक्स्ट में व्याकरणिक सहीपन बनाये रखने, या जनरेट किये गए डेटा की पठनीयता में सुधार के लिए करते हैं।

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
