---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases:
- /hi/clojure/converting-a-string-to-lower-case/
date:                  2024-01-20T17:38:10.776351-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

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
