---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Clojure: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक अस्थायी फाइल बनाना यह एक तकनीक है जो कोडिंग में उपयोग किया जाता है। इसका मुख्य उद्देश्य यह होता है कि जब हमारे प्रोग्राम एक फाइल को स्थायी रूप से संचित नहीं कर सकते हैं, तो हम एक अस्थायी फाइल बनाते हैं।

## कैसे करें:
यहां, हम एक अस्थायी फाइल कैसे बनाएं इसके बारे में कुछ कोडिंग उदाहरण देखेंगे।

उदाहरण १:
```Clojure
(with-open [temp-file (java.io.File/createTempFile "prefix" "suffix")]
  (spit temp-file "This is a temporary file.")
  (println (slurp temp-file)))
;; आउटपुट: This is a temporary file.
```

उदाहरण २:
```Clojure
(with-open [temp-file (java.io.File/createTempFile "prefix" nil)]
  (let [file-writer (clojure.java.io/writer temp-file)]
    (binding [*out* file-writer]
      (println "This is a temporary file.")
      (println "Another line added."))))
;; आउटपुट:
This is a temporary file.
Another line added.
```

## गहराई में जाएं:
अब आप जानते हैं कि अस्थायी फाइल क्या है और हम इसे क्यों घरघराते हैं, आइए इसके गहराई में जाते हैं और इसके पीछे की कुछ और जानकारी प्राप्त करें।

1. इतिहासिक संदर्भ: अस्थायी फाइलों का उपयोग पहले से ही किया जाता था, लेकिन यह शुरुआत में सिस्टम में फाइल लेखन के साथ समस्याओं को हल करने के लिए ही होता था।

2. विकल्प: अस्थायी फाइल बनाने के लिए Clojure में एक और तकनीक है, जहां हम फाइल को अस्थाई रूप से लेखन के लिए mmap (memory-mapped file) का उपयोग कर सकते हैं।

3. प्रयोजन का विवरण: अस्थायी फाइल बनाने के लिए Clojure में java.io.File और java.nio.file.Files श्रेणियों का उपयोग किया जाता है।

## जुड़े रहें:
अस्थायी फाइलों के बारे में अधिक जानने के लिए निम्नलिखित स्रोतों को जांचें:

- [Clojure डॉक्यूमेंटेशन](https://clojuredocs.org/clojure.java.io/create-temp-file)
- [Java डॉक्यूमेंटेशन](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile%28java.lang.String,%20java.lang.String,%20java.io.File%29)
- [Stack Overflow चर्चाएं](https://stackoverflow.com/questions/8209998/temporary-files-in-java)