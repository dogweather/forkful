---
title:                "Clojure: कंप्यूटर प्रोग्रामिंग में json के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में json के साथ काम करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों
JSON (JavaScript Object Notation) एक popular data format है जिसे प्रोग्रामिंग में इस्तेमाल किया जाता है। यह डेटा को एकीकृत ढंग से संग्रहीत करने और इसे स्थानीय भाषाओं में पार्स करने के लिए उपयोगी है। इसलिए, Clojure programmers के लिए JSON को सीखना जरूरी है।

## कैसे करें
```Clojure
;; जैसे ही JSON को Clojure भाषा में पार्स किया जाता है, यह Clojure में मानचित्र में रूपांतरित हो जाता है।
(def json-data
  "{\"name\":\"John\",\"age\":30,\"hobbies\":[\"reading\",\"music\",\"coding\"]}") ;string को डेटा संरचना में परिवर्तित करें

(require '[clojure.data.json :as json]) ;clojure.data.json नामी सामान्य नामइकृत-निर्वाचित namespace को सम्मिलित करें

(json/read-str json-data) ;डेटा parse करें
;; आउटपुट:
;; {"name" "John", "age" 30, "hobbies" ["reading" "music" "coding"]}
```

## गहराई में
JSON को Clojure में काम करने के लिए, clojure.data.json namespace को import करने के अलावा, हम clojure.core में उपलब्ध फ़ंक्शंस का भी उपयोग कर सकते हैं। इनमें से कुछ मुख्य हैं `json/write-str`, `json/generate-string`, `json/read`, `json/write` और `json/parse-string`। हम इन फ़ंक्शंस का उपयोग करके JSON विलक्षणताओं को अधिक घहराने वाले कामों में उपयोगी बना सकते हैं।

## इससे जुड़े लिंक्स
- [Clojure Docs: JSON](https://clojure.org/guides/json)
- [Clojure Docs: clojure.data.json](https://clojure.github.io/clojure/clojure.data.json-api.html)
- [JSON व्याख्या](https://json.org/)