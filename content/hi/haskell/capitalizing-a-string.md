---
title:    "Haskell: स्ट्रिंग केपिटलाइज़ करना"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## इसलिए
कोड लिखने के बाद, यह आमतौर पर अहम होता है कि उसको बेहतर और स्पष्टतर बनाने के लिए स्ट्रिंग या वाक्यों को कैपिटलाइज किया जाए।

## कैसे करें
इस अनुच्छेद में, हम हास्केल में स्ट्रिंग को कैपिटलाइज करने के लिए कॉडिंग उदाहरण देखेंगे।
```Haskell
module Main where

import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = map toUpper str

main :: IO()
main = do
  putStrLn "Enter a string: "
  str <- getLine
  let capitalized = capitalize str
  putStrLn ("Capitalized string: " ++ capitalized)
```

उत्पाद:
```Haskell
Enter a string:
haskell programming
Capitalized string: HASKELL PROGRAMMING
```

## गहराई में जाएं
यदि हम गहराई में जाएं, तो हम देखेंगे कि हास्केल भाषा में `capitalize` फ़ंक्शन स्ट्रिंग को कैपिटलाइज करने के लिए `map` फ़ंक्शन का उपयोग करता है। `Data.Char` मॉड्यूल स्टान्डर्ड लाइब्रेरी में उपलब्ध होता है और यह एक स्ट्रिंग के हर अक्षर को अपरकेस में बदलने के लिए `toUpper` फ़ंक्शन का उपयोग करता है। हम `capitalize` फ़ंक्शन को `[Char] -> [Char]` की सामान्य टाइप से भी लिख सकते हैं। इससे हम अन्य डेटा टाइप्स में भी इसका उपयोग कर सकते हैं, जैसे लिस्टिंग  या टक्स्ट।

## देखें भी
- [अनिकेत कुमार का ब्लॉग पोस्ट](https://aniketkumar.com/blog/haskell-string-manipulation/)
- [हास्केल डॉक्यूमेंटेशन](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.16.0.0/Data-Char.html#v:toUpper)