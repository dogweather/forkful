---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट फाइल लिखना यानी कि डाटा को टेक्स्ट के रूप में फाइल में सेव करना। प्रोग्रामर्स डेटा को सेव करने, कॉन्फ़िगरेशन सेटिंग्स स्टोर करने, या लॉग्ज़ बनाने के लिए इसे करते हैं।

## कैसे करें:
Clojure में टेक्स्ट फाइल लिखने का एक साधारण उदहारण:
```clojure
(with-open [wrtr (java.io.BufferedWriter. (java.io.FileWriter. "test.txt"))]
  (.write wrtr "Hello, Clojure!"))
```
इस कोड से `test.txt` नामक फाइल बनेगी और उसमें "Hello, Clojure!" लिखा होगा।

## गहराई में:
Clojure में फाइल सिस्टम के साथ काम करते समय जावा की फाइल एपीआई का उपयोग होता है क्योंकि Clojure जावा प्लैटफॉर्म पर चलता है। इसके अल्टरनेटिव्स में `slurp` और `spit` जैसे Clojure के बिल्ट-इन फंक्शन हैं जो डाटा पढ़ने और लिखने का काम और भी आसान बना देते हैं। इम्प्लीमेंटेशन डिटेल में कोड को अधिक अच्छे से हैंडल करने के लिए exception handling और रिसोर्स मैनेजमेंट पर ध्यान देना होता है।

## इसे भी देखें:
- Clojure डोक्यूमेंटेशन: [https://clojure.org/](https://clojure.org/)
- Java IO Tutorial: [https://docs.oracle.com/javase/tutorial/essential/io/](https://docs.oracle.com/javase/tutorial/essential/io/)
- ClojureDocs, एक कम्युनिटी ड्रिवन डोक्यूमेंटेशन रिसोर्स: [https://clojuredocs.org/](https://clojuredocs.org/)
