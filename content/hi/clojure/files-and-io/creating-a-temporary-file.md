---
date: 2024-01-20 17:40:24.989823-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:51.696726-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

## How to: (कैसे करें:)
```Clojure
; Clojure के लिए एक अस्थायी फाइल बनाने का उदाहरण:

; java.io.File की ज़रूरत होती है
(import 'java.io.File)

; अस्थायी फाइल बनाना
(defn create-temp-file []
  (let [temp-file (File/createTempFile "temp" ".txt")]
    (println "अस्थायी फाइल बनाई गई: " temp-file.getAbsolutePath)
    temp-file))

; फ़ंक्शन को कॉल करें
(create-temp-file)
```

सैंपल आउटपुट कुछ इस तरह होगा:
```
अस्थायी फाइल बनाई गई: /tmp/temp1234567890.txt
```

## Deep Dive (गहराई से जानकारी)
अस्थायी फाइलें कंप्यूटर सिस्टम में काफी पुरानी अवधारणा हैं। ये यूनिक्स सिस्टम के समय से हैं, जब डेटा का अस्थायी संग्रहण अक्सर `/tmp` डायरेक्ट्री में किया जाता था। क्लोजर, जो JVM पर चलता है, `java.io.File` क्लास का उपयोग करता है ताकि यह कार्य कर सके। विकल्प के रूप में, `nio` पैकेज भी है जो अधिक उन्नत फाइल-हैंडलिंग प्रदान करता है। बात करें तो Clojure में, यह सभी कार्य जावा के साथ शक्तिशाली सहजीवन में किए जाते हैं।

## See Also (और भी देखें)
- Clojure के अधिकारिक दस्तावेज: [Clojure Docs](https://clojure.org/api/api)
- `java.io.File` का जावा दस्तावेज: [Java 7 File I/O](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- `java.nio.file` संक्षेप विवरण: [Overview of java.nio.file package](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html)
