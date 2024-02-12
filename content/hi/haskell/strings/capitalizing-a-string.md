---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases: - /hi/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:51.839516-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
