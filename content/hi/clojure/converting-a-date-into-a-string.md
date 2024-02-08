---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:36:24.919638-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख को स्ट्रिंग में बदलना उसे पढ़ने योग्य फॉर्मेट में सेट करना होता है। प्रोग्रामर्स डेटा को लॉग फाइलों में सेव करने, यूजर इंटरफेस में दिखाने या अन्य सिस्टम्स में डेटा भेजने के लिए इसे करते हैं।

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
