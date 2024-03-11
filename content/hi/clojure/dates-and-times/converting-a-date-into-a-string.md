---
date: 2024-01-20 17:36:24.919638-07:00
description: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E \u0909\
  \u0938\u0947 \u092A\u0922\u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F \u092B\
  \u0949\u0930\u094D\u092E\u0947\u091F \u092E\u0947\u0902 \u0938\u0947\u091F \u0915\
  \u0930\u0928\u093E \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0921\u0947\u091F\u093E\
  \ \u0915\u094B \u0932\u0949\u0917 \u092B\u093E\u0907\u0932\u094B\u0902 \u092E\u0947\
  \u0902 \u0938\u0947\u0935 \u0915\u0930\u0928\u0947, \u092F\u0942\u091C\u0930 \u0907\
  \u0902\u091F\u0930\u092B\u0947\u0938 \u092E\u0947\u0902 \u0926\u093F\u0916\u093E\
  \u0928\u0947 \u092F\u093E \u0905\u0928\u094D\u092F\u2026"
lastmod: '2024-03-11T00:14:25.549422-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E \u0909\u0938\
  \u0947 \u092A\u0922\u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F \u092B\u0949\
  \u0930\u094D\u092E\u0947\u091F \u092E\u0947\u0902 \u0938\u0947\u091F \u0915\u0930\
  \u0928\u093E \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0921\u0947\u091F\u093E \u0915\
  \u094B \u0932\u0949\u0917 \u092B\u093E\u0907\u0932\u094B\u0902 \u092E\u0947\u0902\
  \ \u0938\u0947\u0935 \u0915\u0930\u0928\u0947, \u092F\u0942\u091C\u0930 \u0907\u0902\
  \u091F\u0930\u092B\u0947\u0938 \u092E\u0947\u0902 \u0926\u093F\u0916\u093E\u0928\
  \u0947 \u092F\u093E \u0905\u0928\u094D\u092F\u2026"
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
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
