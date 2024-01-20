---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "Haskell: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वर्णमाला को बड़ा करना (capitalizing a string) से मतलब है प्रत्येक शब्द के पहले अक्षर को बड़े अक्षर में परिवर्तित करना। फ़िर यह सवाल उठता है कि प्रोग्रामर्स ऐसा क्यों करते हैं? यह तब किया जाता है जब हमें उपयोगकर्ता इनपुट को विन्यास-संवेदनशील (format-sensitive) तरीके से साद्धारित करने की आवश्यकता होती है।

## कैसे करें:

हास्केल में, हम `Data.Char` मॉड्यूल का उपयोग करके `toUpper` फ़ंक्शन के साथ कोई भी इनपुट स्ट्रिंग को कैपिटलाइज़ कर सकते हैं। 

```Haskell
import Data.Char

capitalize :: String -> String
capitalize = unwords . map (capitalizeWord) . words
    where
        capitalizeWord [] = []
        capitalizeWord (x:xs) = toUpper x : map toLower xs
      
main = print(capitalize "hello world")
```
इसका स्क्रीन पर प्रिंट होने वाला आउटपुट होगा: "Hello World".

## गहरा डाइव:

#### ऐतिहासिक प्रसंग:
`toUpper` फ़ंक्शन का उपयोग करने वाले कोड की विचारणीयता हास्केल 98 के निर्माण के समय से ही थी। यह विस्तार के लिए डिज़ाइन किया गया था, जो यह सुनिश्चित करता है कि यह सामर्थ्य और उपयोगकर्ता मानकों के अनुसार स्केल कर सके।

#### वैकल्पिक:
म्यूटेबल विकल्प में, `Data.Text` का `toUpper` फ़ंक्शन उपयोग कर सकते हैं।

#### कार्यान्वयन विवरण:
पहले `words` फ़ंक्शन इनपुट स्ट्रिंग को शब्दों में विभाजित करता है, फिर `map` फ़ंक्शन `capitalizeWord` को प्रत्येक शब्द पर लागू करता है, और अंत में `unwords` फ़ंक्शन शब्दों को फिर से एक स्ट्रिंग में जोड़ता है।

## देखें भी:

1. Learn You a Haskell for Great Good: [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
2. Real World Haskell: [http://book.realworldhaskell.org/](http://book.realworldhaskell.org/)
3. Hoogle - Haskell API Search: [https://www.haskell.org/hoogle/](https://www.haskell.org/hoogle/)