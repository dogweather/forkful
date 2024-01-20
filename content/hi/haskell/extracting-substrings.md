---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 1. क्या और क्यों?

Substring एक बारी में कई characters की एक सीक्वेंस होती हैं जो एक बड़ी string से निकाली जाती हैं। कई बार, हमें कुछ विशेष characters की तलाश होती है, तभी हम substring निकालते हैं। 

## 2. कैसे करें:

### substring निकालना: drop और take का प्रयोग करें

```Haskell
let s = "Haskell Programming Language"
let subString = drop 8 . take 18 $ s
print subString
```

आपको "Programming" मिलेगा। 

### substring निकालना: splitAt का प्रयोग करें

```Haskell
let s = "Haskell Programming Language"
let (_, temp) = splitAt 8 s
let (subString, _) = splitAt 10 temp
print subString
```

आपको "Programming" मिलेगा। 

## 3. गहराई में:

### ऐतिहासिक संदर्भ:

Haskell एक pure functional programming language है और इसकी सबस्ट्रिंग निकालने की क्षमता पुराने समय से ही है।

### विकल्प:

Python और JavaScript जैसी programming languages में, slicing operator का उपयोग करके मानचित्रण किया जा सकता है। 

### बातचीत:

Haskell में Substrings को निकालने के लिए `take`, `drop और `splitAt` का इस्तेमाल किया जा सकता है। 

## 4. देखें भी:

- [Haskell Prelude](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html): Haskell के prelude डॉक्यूमेंटेशन की अधिक जानकारी के लिए।
- [Hoogle](https://www.haskell.org/hoogle/): Haskell लाइब्रेरी के फंक्शंस खोजने के लिए हूगल का प्रयोग करें।