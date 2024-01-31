---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:35:55.059451-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पार्सिंग एक स्ट्रिंग से डेट का मतलब होता है स्ट्रिंग में मौजूद तारीखों को पहचानना और उन्हें संग्रहित करना। प्रोग्रामर्स इसे डेटा वैलिडेशन, सर्टिफिकेशन, और मैनिपुलेशन के लिए करते हैं।

## How to: (कैसे करें:)
Clojure में डेट पार्स करने के लिए `clj-time` लाइब्रेरी का इस्तेमाल किया जाता है, जिसे `java.time` के समर्थन के साथ अपडेट किया गया है। उदाहरण और उनके आउटपुट देखिए:

```Clojure
(require '[java.time.format :refer [DateTimeFormatter]])
(require '[java.time :as jt])

(defn parse-date [date-string]
  (let [formatter (DateTimeFormatter/ofPattern "dd-MM-yyyy")]
    (.format (jt/parse date-string formatter))))

;; Example usage:
(parse-date "23-05-2021")
;; Output: "2021-05-23"
```

इस कोड से आप "23-05-2021" फॉर्मेट वाली स्ट्रिंग को ISO-8601 फॉर्मेट में पार्स कर सकेंगे।

## Deep Dive (गहराई में जानकारी)
डेट पार्सिंग, पूर्व में पुराने `java.util.Date` और `java.util.Calendar` पर निर्भर थी। जावा 8 में `java.time` API लाई गई, जिसे Clojure सामुदायिक लाइब्रेरीज जैसे कि `clj-time` ने शामिल किया। इससे पार्सिंग अधिक लचीली और विश्वसनीय हो गई है।

पार्सिंग के लिए विकल्प में बाहरी लाइब्रेरीज जैसे कि `Joda-Time` शामिल हैं, पर `java.time` के साथ इसकी आवश्यकता कम हो गई है।

इम्प्लीमेंटेशन डिटेल्स पर गौर करें तो, पार्सिंग के दौरान फॉर्मेटर्स का इस्तेमाल करके स्ट्रिंग को सही तरीके से पार्स करना होता है। इसके लिए पैटर्न स्ट्रिंग बहुत महत्वपूर्ण होता है।

## See Also (और भी जानकारी)
- java.time API के दस्तावेज: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- clj-time GitHub पृष्ठ: https://github.com/clj-time/clj-time
- Clojure डॉक्स का `java-time` लाइब्रेरी पर पृष्ठ: https://clojuredocs.org/clojure.java-time
