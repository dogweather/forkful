---
date: 2024-01-20 17:38:10.776351-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0928\
  \u093F\u091A\u0932\u0947 \u0915\u0947\u0938 \u092E\u0947\u0902 \u092C\u0926\u0932\
  \u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0930 \u0905\u0915\u094D\u0937\u0930\
  \ \u0915\u094B \u091B\u094B\u091F\u093E \u0915\u0930\u0928\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0924\
  \u0941\u0932\u0928\u093E, \u0916\u094B\u091C \u0914\u0930 \u0921\u0947\u091F\u093E\
  \ \u0938\u093E\u092B-\u0938\u0941\u0925\u0930\u093E \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F \u0910\u0938\u093E \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902\u0964"
lastmod: '2024-03-13T22:44:51.640820-06:00'
model: gpt-4-1106-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0928\u093F\
  \u091A\u0932\u0947 \u0915\u0947\u0938 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\
  \u093E \u092E\u0924\u0932\u092C \u0939\u0930 \u0905\u0915\u094D\u0937\u0930 \u0915\
  \u094B \u091B\u094B\u091F\u093E \u0915\u0930\u0928\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0924\u0941\u0932\
  \u0928\u093E, \u0916\u094B\u091C \u0914\u0930 \u0921\u0947\u091F\u093E \u0938\u093E\
  \u092B-\u0938\u0941\u0925\u0930\u093E \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u0910\u0938\u093E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964\
  ."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

## What & Why? (क्या और क्यों?)
स्ट्रिंग को निचले केस में बदलना मतलब हर अक्षर को छोटा करना है। प्रोग्रामर्स तुलना, खोज और डेटा साफ-सुथरा करने के लिए ऐसा करते हैं।

## How to: (कैसे करें:)
Clojure में स्ट्रिंग को निचले केस में बदलना बहुत सीधा है। यहाँ पर एक उदाहरण है:

```Clojure
(defn to-lower-case [str]
  (.toLowerCase str))

(println (to-lower-case "नमस्ते, CLOJURE!")) ; output: "नमस्ते, clojure!"
```

`toLowerCase` जावा मेथड का प्रयोग करके हम आसानी से किसी भी स्ट्रिंग को छोटे अक्षरों में बदल सकते हैं।

## Deep Dive (गहराई से जानकारी)
स्ट्रिंग को निचले केस में बदलना जावा के `toLowerCase` मेथड के जरिये Clojure में आता है क्योंकि Clojure JVM पर चलता है और जावा लाइब्रेरीज़ का इस्तेमाल कर सकता है। अलग-अलग भाषाएँ और लोकेल्स को सपोर्ट करने के लिए, जावा में `Locale` का भी प्रावधान है। Clojure में यह सरलता और लचीलापन देता है।

विकल्पों की बात करें, तो `clojure.string` लाइब्रेरी में `lower-case` फ़ंक्शन भी है जो एकदम सीधा है:

```Clojure
(require '[clojure.string :as str])

(println (str/lower-case "नमस्ते, CLOJURE!")) ; output: "नमस्ते, clojure!"
```

बड़ी स्ट्रिंग्स के साथ काम करते समय प्रदर्शन का भी ध्यान रखना पड़ता है, खासकर जब इसे बार-बार करना पड़ता है।

## See Also (और भी देखें)
1. Clojure Docs for String Manipulation: [clojuredocs.org](https://clojuredocs.org/clojure.string)
2. Java Locale and Unicode: [Oracle Locale Docs](https://docs.oracle.com/javase/tutorial/i18n/locale/)
3. Clojure और Java interoperability के लिए गाइड: [Clojure Java Interop](https://clojure.org/reference/java_interop)
