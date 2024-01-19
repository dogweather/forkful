---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वर्त्तमान तारीख प्राप्त करना का अर्थ होता है, सटीक समय व दिनांक प्राप्त करना।प्रोग्रामर्स यह इसलिए करते हैं ताकि वे डाटा को समय के साथ संबंधित कर सकें और समय पर आधारित प्रवाहों को मॉनिटर कर सकें।

## कैसे करें:

Clojure में, वर्तमान तारीख प्राप्त करने के लिए मैं आपको java.time.LocalDate का use करने की सलाह देता हूँ। 

```
(defn get-current-date []
  (.toString (java.time.LocalDate/now)))
```

जब आप इसे चलाते हैं:

```
(get-current-date)
```

आपको मिलेगा:

```
"2021-06-10"
```
## गहरी डुबकी:

वर्तमान तारीख प्राप्त करने के लिए Java.time API का सर्वश्रेष्ठ उपयोग करने की सिफारिश की जाती है, जो Java 8 में जोड़ी गई। Java.util.date और java.util.calendar के ऐतिहासिक उपयोग के मुकाबले में, यह अधिक समझ के लायक और सुगम है।

## यह भी देखें:

अधिक जानकारी के लिए, आपको कुछ उपयोगी संसाधन मिल सकते हैं:

- [Clojure की आधिकारिक वेबसाइट](https://www.clojure.org/)
- [Java.time API documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Practical Clojure बुक](http://www.apress.com/9781430272311)