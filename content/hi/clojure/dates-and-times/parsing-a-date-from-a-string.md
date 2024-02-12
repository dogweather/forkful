---
title:                "स्ट्रिंग से तारीख पार्स करना"
aliases:
- hi/clojure/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:49.797424-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Clojure में एक स्ट्रिंग से दिनांक पार्स करना दिनांक और समय के पाठ्य अभिव्यक्तियों को एक अधिक उपयोगी रूप में परिवर्तित करने के बारे में है (उदाहरण के लिए, Clojure का DateTime ऑब्जेक्ट)। यह प्रक्रिया डेटा प्रसंस्करण, लॉगिंग, या किसी भी अनुप्रयोग के लिए मौलिक है जो कालानुक्रमिक डेटा का संचालन कर रहा है, जिससे प्रोग्रामर दिनांकों पर कुशलतापूर्वक कार्य, तुलना, या हेरफेर कार्य कर सकते हैं।

## कैसे करें:
Clojure, एक JVM भाषा होने के नाते, आपको Java की दिनांक और समय लाइब्रेरीज़ का सीधे उपयोग करने की अनुमति देता है। चलिए Java इंटरऑपरेशन के बिल्ड-इन इस्तेमाल से शुरुआत करते हैं और फिर देखते हैं कि कैसे एक लोकप्रिय तीसरे पक्ष की लाइब्रेरी, clj-time, का उपयोग करके और अधिक idiomatic Clojure समाधानों का उपयोग करें।

### Java इंटरऑप का उपयोग करना
Clojure सीधे Java की `java.time.LocalDate` का उपयोग करके स्ट्रिंग्स से दिनांक पार्स कर सकता है:
```clojure
(require '[clojure.java.io :as io])

; Java इंटरऑप का उपयोग करके एक दिनांक पार्स करना
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; आउटपुट: 2023-04-01
```

### clj-time का उपयोग करना
दिनांक और समय के साथ काम करने के लिए एक और अधिक idiomatic Clojure लाइब्रेरी `clj-time` है। यह Joda-Time को लपेटती है, जो दिनांक और समय संचालन के लिए एक व्यापक लाइब्रेरी है। सबसे पहले, आपको अपने प्रोजेक्ट की निर्भरताओं में `clj-time` जोड़ने की आवश्यकता होगी। यहाँ पर एक दिनांक स्ट्रिंग को पार्स करने के लिए `clj-time` का उपयोग कैसे करें:

```clojure
; सुनिश्चित करें कि आपकी project.clj में :dependencies के अंतर्गत [clj-time "0.15.2"] जोड़ दिया गया है

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; एक फॉर्मेटर निर्धारित करें
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; आउटपुट: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

ये उदाहरण मूल दिनांक पार्सिंग दिखाते हैं। दोनों विधियां उपयोगी हैं, लेकिन `clj-time` जटिल आवश्यकताओं के लिए अतिरिक्त कार्यक्षमताओं के साथ एक और अधिक Clojure-centric दृष्टिकोण प्रदान कर सकती है।
