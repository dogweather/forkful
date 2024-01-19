---
title:                "एक स्ट्रिंग को बड़े अक्षरों में बदलना"
html_title:           "Clojure: एक स्ट्रिंग को बड़े अक्षरों में बदलना"
simple_title:         "एक स्ट्रिंग को बड़े अक्षरों में बदलना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वर्णमाला में भागों को बड़ा करना होता है, जैसे कि "hello" को बदलकर "HELLO" । यह काम करना उपयोगी होता है क्योंकि बार-बार कुछ भी बड़ा या छोटा करने के लिए यह तैयार नहीं होते।

## कैसे करें:

आप Clojure के `upper-case` फ़ंक्शन का उपयोग करके स्ट्रिंग्स को कैपिटलाइज़ कर सकते हैं। 

```Clojure
;; Clojure example
(clojure.string/upper-case "hello world") ;; Outputs: "HELLO WORLD"
```

## गहनावरण:

१. ऐतिहासिक प्रसंग: Clojure 1.3 के रिलीज के साथ 2011 में, `clojure.string/upper-case` फ़ंक्शन का परिचय दिया गया। इससे पहले, कोई बिल्ट-इन फ़ंक्शन नहीं होता था, और कोडर्स को यह स्वयं लागू करना पड़ता।

२. विकल्प: Clojure के एकेरी "java interop" फ़ंक्शन का उपयोग करके, आप जावा के `toUpperCase` मेथोड का उपयोग कर सकते हैं। 

```Clojure
;; Clojure example
(.toUpperCase "hello world") ;; Outputs: "HELLO WORLD"
```

३. क्रियान्वयन विवरण: `clojure.string/upper-case` फ़ंक्शन का पीछा करें, आपको `java.lang.String` के `toUpperCase` मेथड के पास फ़ंक्शन को ले जाएगा। 

## और देखें:

1. [Clojure Documentation on clojure.string/upper-case](https://clojuredocs.org/clojure.string/upper-case) - एक अधिक विस्तृत वर्णन और उदाहरण देखने के लिए।
2. [Java Documentation on String.toUpperCase](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--) - जावा कार्यान्वयन विवरण देखने के लिए।