---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:47.285915-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रैंडम नंबर जनरेशन यानि अनियमित संख्याओं का निर्माण एक ऐसी प्रक्रिया है जिससे हम अप्रत्याशित संख्याएँ प्राप्त कर सकते हैं। प्रोग्रामर्स इसका इस्तेमाल खेलों, एन्क्रिप्शन और सिमुलेशन जैसे कार्यों में करते हैं ताकि वास्तविकता की अनुभूति हो।

## कैसे करें:
```Haskell
import System.Random (randomRIO)

-- रैंडम नंबर जनरेट करना
randomNumber :: IO Int
randomNumber = randomRIO (1, 10) -- 1 से 10 के बीच एक रैंडम नंबर

main :: IO ()
main = do
    num <- randomNumber
    putStrLn ("आपका रैंडम नंबर: " ++ show num)
```
उदाहरण आउटपुट:
```
आपका रैंडम नंबर: 7
```

## गहराई से समझ:
रैंडम नंबर्स को जेनेरेट करने का इतिहास उतना ही पुराना है जितना कि कंप्यूटर विज्ञान का। अधिकतर रैंडम नंबर जेनेरेटर्स (आरएनजी) प्रकृति में प्स्यूडोरैंडम होते हैं, जिसका अर्थ है कि वे पूर्णतः अनियमित नहीं होते हैं बल्कि एक निश्चित एल्गोरिथम का अनुसरण करते हैं। हस्केल में `System.Random` मॉड्यूल इस्तेमाल करके हम आसानी से रैंडम संख्याएँ जेनेरेट कर सकते हैं और उसे पूरी तरह से 'अनियमित' माना जाता है इस्तेमालकर्ताओं के परिपेक्ष्य में। वैकल्पिक लाइब्रेरीज में `random-fu`, `mwc-random` आदि शामिल हैं, जो विभिन्न तरह के एल्गोरिथम्स और उन्नत सुविधाओं को प्रदान करती हैं।

## यह भी देखें:
- Haskell `System.Random` मॉड्यूल: [Hackage System.Random](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- प्स्यूडोरैंडम नंबर जनरेटर्स के बारे में जानकारी: [Wikipedia PRNG](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- `random-fu` लाइब्रेरी: [Hackage random-fu](https://hackage.haskell.org/package/random-fu)
- `mwc-random` लाइब्रेरी: [Hackage mwc-random](https://hackage.haskell.org/package/mwc-random)