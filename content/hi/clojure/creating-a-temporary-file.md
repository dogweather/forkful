---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

अस्थायी फ़ाइल बनाना मतलब एक अधिसूचना सुरक्षित/अस्थायी भंडारण स्थल पर डेटा रखना। कोडर्स इसे कई कारणों से करते हैं - मुख्यतः जब वे अस्थायी डेटा (जैसे - पीढ़ी के परिणाम) संग्रहित करने के इच्छुक होते हैं इसका उपयोग करते हैं।

## कैसे करें:

आप Clojure के `java.io.File/createTempFile` का उपयोग करके अस्थायी फ़ाइलें तैयार कर सकते हैं। यहां पर एक उदाहरण है:

```clojure
(import 'java.io.File)

(def tempfile (File/createTempFile "prefix" "suffix")) 

(println tempfile) ; print the temporary file path
```

यह स्क्रिप्ट एक अस्थायी फ़ाइल बनाती है और फ़ाइल का पथ प्रिंट करती है।

## गहरा दौरा:

अस्थायी फ़ाइलों का उपयोग डैटा को अस्तित्व में रखने के लिए किया जाता है, जब तक कि वह आवश्यक न हो जाए। इसके ऐतिहासिक प्रक्षेपण में, यह एक उचित उपाय था जब कंप्यूटर में स्थायी संग्रहण स्थल की कमी होती थी। वैकल्पिक रूप से, एप्लिकेशन डाटा को in-memory डेटा संरचनाओं में संग्रहीत कर सकते हैं, यदि वे इसे अस्थायी मानते हैं। अस्थायी फ़ाइलें JVM (Java Virtual Machine) पर काम करने के लिए क्लोजर को केवल `java.io` लाइब्रेरी का उपयोग करते हुए बनाई जाती हैं।

## देखने के लिए भी:

- क्लोजर डॉक्यूमेंटेशन ([link](https://clojure.org/api/api))
- Java टेम्प फ़ाइल API ([link](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html#createTempFile(java.nio.file.Path,%20java.lang.String,%20java.lang.String,%20java.nio.file.attribute.FileAttribute...)))
  
यदि आप क्लोजर में नए हैं तो इन लिंक्स की परीक्षा लेने से आपको बहुत फ़ायदा होगा।