---
date: 2024-01-26 04:15:34.662997-07:00
description: "\u0939\u093E\u0938\u094D\u0915\u0947\u0932 \u092E\u0947\u0902 \u090F\
  \u0915 \u0907\u0902\u091F\u0930\u0948\u0915\u094D\u091F\u093F\u0935 \u0936\u0947\
  \u0932, \u092F\u093E REPL (Read-Eval-Print Loop), \u0906\u092A\u0915\u094B \u0932\
  \u093E\u0907\u0935 \u0915\u094B\u0921 \u0938\u094D\u0928\u093F\u092A\u0947\u091F\
  \u094D\u0938 \u091A\u0932\u093E\u0928\u0947 \u0926\u0947\u0924\u093E \u0939\u0948\
  \u0964 \u092F\u0939 \u0924\u094D\u0935\u0930\u093F\u0924 \u092A\u094D\u0930\u0924\
  \u093F\u0915\u094D\u0930\u093F\u092F\u093E, \u092B\u0902\u0915\u094D\u0936\u0928\
  \u094D\u0938 \u0915\u0940 \u092A\u0930\u0940\u0915\u094D\u0937\u0923, \u0914\u0930\
  \ \u092D\u093E\u0937\u093E\u2026"
lastmod: '2024-03-13T22:44:52.407419-06:00'
model: gpt-4-0125-preview
summary: "\u0939\u093E\u0938\u094D\u0915\u0947\u0932 \u092E\u0947\u0902 \u090F\u0915\
  \ \u0907\u0902\u091F\u0930\u0948\u0915\u094D\u091F\u093F\u0935 \u0936\u0947\u0932\
  , \u092F\u093E REPL (Read-Eval-Print Loop), \u0906\u092A\u0915\u094B \u0932\u093E\
  \u0907\u0935 \u0915\u094B\u0921 \u0938\u094D\u0928\u093F\u092A\u0947\u091F\u094D\
  \u0938 \u091A\u0932\u093E\u0928\u0947 \u0926\u0947\u0924\u093E \u0939\u0948\u0964\
  \ \u092F\u0939 \u0924\u094D\u0935\u0930\u093F\u0924 \u092A\u094D\u0930\u0924\u093F\
  \u0915\u094D\u0930\u093F\u092F\u093E, \u092B\u0902\u0915\u094D\u0936\u0928\u094D\
  \u0938 \u0915\u0940 \u092A\u0930\u0940\u0915\u094D\u0937\u0923, \u0914\u0930 \u092D\
  \u093E\u0937\u093E\u2026"
title: "\u0907\u0902\u091F\u0930\u0948\u0915\u094D\u091F\u093F\u0935 \u0936\u0947\u0932\
  \ (REPL) \u0915\u093E \u0909\u092A\u092F\u094B\u0917"
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
