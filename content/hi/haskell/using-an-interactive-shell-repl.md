---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
date:                  2024-01-26T04:15:34.662997-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"

category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
हास्केल में एक इंटरैक्टिव शेल, या REPL (Read-Eval-Print Loop), आपको लाइव कोड स्निपेट्स चलाने देता है। यह त्वरित प्रतिक्रिया, फंक्शन्स की परीक्षण, और भाषा सीखने के लिए एक प्रयोगशाला है।

## कैसे:
GHCi (ग्लासगो हास्केल कम्पाइलर का इंटरैक्टिव वातावरण) शुरू करने के लिए, बस अपने टर्मिनल में `ghci` टाइप करें। इसका उपयोग कैसे करें, यहाँ देखें:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

नमूना आउटपुट बताता है कि `x` एक संख्यात्मक वेरिएबल है और इसे दोगुना करने पर परिणाम 10 होता है।

## गहराई से:
हास्केल का GHCi अपने शुरुआती समय से बहुत आगे निकल चुका है। इसमें टैब कंपलीशन, मल्टी-लाइन इनपुट, और पैकेज लोडिंग जैसी धनी फीचर्स सेट प्रदान करता है। Hugs जैसे विकल्प अब अधिकतर ऐतिहासिक हो चुके हैं, GHCi मानक बन गया है। GHCi हर बार जब आप एक एक्सप्रेशन दर्ज करते हैं तो कोड को जस्ट-इन-टाइम कंपाइल करता है, आपको अपने हास्केल कोड का परीक्षण करने का एक कुशल तरीका प्रदान करता है।

## देखें भी:
- [द जीएचसी यूजर्स गाइड – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [लर्न यू ए हास्केल फॉर ग्रेट गुड! – स्टार्टिंग आउट](http://learnyouahaskell.com/starting-out#hello-world)
- [हास्केल विकी – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
