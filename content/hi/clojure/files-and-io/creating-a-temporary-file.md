---
title:                "अस्थायी फाइल बनाना"
aliases:
- /hi/clojure/creating-a-temporary-file/
date:                  2024-01-20T17:40:24.989823-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
अस्थायी फाइल बनाना एक ऐसी प्रक्रिया है जो अस्थायी डाटा संग्रहीत करने के लिए एक फाइल का निर्माण करती है। प्रोग्रामर्स इसका उपयोग डेटा को ट्रांसफर, टेस्टिंग या डीबगिंग के दौरान करते हैं।

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
