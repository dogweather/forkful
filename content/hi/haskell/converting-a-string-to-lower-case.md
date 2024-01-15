---
title:                "स्ट्रिंग को छोटे अक्षर में रूपांतरित करना"
html_title:           "Haskell: स्ट्रिंग को छोटे अक्षर में रूपांतरित करना"
simple_title:         "स्ट्रिंग को छोटे अक्षर में रूपांतरित करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी प्रोग्रामर कोई भी भाषा सीखते हुए अपने कार्यों को सरल बनाने का प्रयास करता है। इसलिए, मूल रूप से वह अपने कोड को सबसे प्रभावी तरीके से लिखना चाहता है। स्ट्रिंग को लोअर केस में रूपांतरित करने से संस्करण और कॉपी/पेस्ट आसान होता है। यह भी पढ़ें: [लघुक्रम क्या होता है और वास्तव में इसे लिखना क्यों आसान होता है](https://www.haskell.org/).

## कैसे करें

स्ट्रिंग को लोअर केस में रूपांतरित करने के लिए, हास्केल में `toLower` फंक्शन का प्रयोग किया जाता है। इसका उपयोग निम्न आदेशिकता के साथ किया जाता है:

```Haskell
toLower :: Char -> Char
```

जहाँ `toLower` एक वर्ग से वर्ग परिवर्तन करता है, और `Char` वर्ग वास्तव में एक एनुमेरेबल एवं चरित्र मनचाहे वाक्यांश में होता है। आइसो बर के साथ, हास्केल में एक सरल रीसोर्स है जो पुरोहित और एनोंपरान प्रदान करता है जो कि आप चाहते हैं और चाहते हैं. कि आप क्यों विकसित कर रहे हैं।

यह हमारे स्ट्रिंग को लोअर केस में रूपांतरित करने का एक उदाहरण है:

```Haskell
toLower 'H' -- output: 'h'
toLower 'E' -- output: 'e'
toLower 'l' -- output: 'l'
toLower 'L' -- output: 'l'
toLower 'O' -- output: 'o'
```

यदि हम `module Main` के साथ अपना कोड रखते हैं, तो हम निम्न रूप से भी लिख सकते हैं:

```Haskell
module Main where

import Data.Char (toLower)

main :: IO ()
main = do
  putStrLn (toLower 'H') -- output: 'h'
  putStrLn (toLower 'E') -- output: 'e'
  putStrLn (toLower 'l') -- output: 'l'
  putStrLn (toLower 'L') -- output: 'l'