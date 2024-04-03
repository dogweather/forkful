---
date: 2024-01-20 17:36:24.919638-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Clojure\
  \ \u092E\u0947\u0902 \u090F\u0915 \u0938\u093E\u0927\u093E\u0930\u0923 date-to-string\
  \ \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u092C\u0928\u093E\u0915\u0930 \u0926\
  \u0947\u0916\u0924\u0947 \u0939\u0948\u0902\u0964."
lastmod: '2024-03-13T22:44:51.683151-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u092E\u0947\u0902 \u090F\u0915 \u0938\u093E\u0927\u093E\u0930\u0923\
  \ date-to-string \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u092C\u0928\u093E\u0915\
  \u0930 \u0926\u0947\u0916\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## How to: (कैसे करें:)
Clojure में एक साधारण date-to-string फ़ंक्शन बनाकर देखते हैं।

```Clojure
(import java.text.SimpleDateFormat)
(import java.util.Date)

(defn date-to-string [date format]
  (let [formatter (SimpleDateFormat. format)]
    (.format formatter date)))

;; उदाहरण कॉल:
(def current-date (Date.))
(println (date-to-string current-date "dd-MM-yyyy HH:mm:ss"))
```

सैंपल आउटपुट:

```Clojure
"31-03-2023 16:45:23"
```

## Deep Dive (गहन जानकारी)
क्लोजर, जावा प्लेटफार्म पर चलने वाली एक फ़ंक्शनल प्रोग्रामिंग भाषा है, और java.util.Date तथा SimpleDateFormat जैसे जावा क्लासेस का इस्तेमाल कर सकती है। ये क्लासेस जावा के शुरुआती संस्करणों से मौजूद हैं। हमारे पास java.time (JSR-310) - जो Java 8 में दिखा - जैसे नए अल्टरनेटिव्स भी हैं, जिन्हें आधुनिक और ज्यादा सुधारत्मक माना जाता है। लेकिन, कई बार हमें पुराने APIs का उपयोग करने की जरूरत पड़ती है, खासकर लिगेसी प्रोजेक्ट्स में।

SimpleDateFormat का उपयोग अक्सर अपने फॉर्मेटिंग फीचर्स की वजह से होता है, पर यह धागा-सुरक्षित (thread-safe) नहीं है, जिसके लिए हमें सावधानीपूर्वक सिनक्रोनाइजेशन या थ्रेड-लोकल वैरिएबल्स का उपयोग करना पड़ता है। जबकि, java.time.format.DateTimeFormatter धागा-सुरक्षित है और इसे इस्तेमाल करने की सिफ़ारिश की जाती है, अगर आप Java 8 या उसके बाद के संस्करणों पर काम कर रहे हैं।

## See Also (और भी देखें)
- Clojure Documentation: [https://clojure.org](https://clojure.org)
- Clojure Cheatsheet: [https://clojure.org/api/cheatsheet](https://clojure.org/api/cheatsheet)
- Java SimpleDateFormat documentation: [https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- Java 8 DateTimeFormatter guide: [https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
