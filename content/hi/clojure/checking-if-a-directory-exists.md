---
title:                "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
html_title:           "Arduino: यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
simple_title:         "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी का अस्तित्व जांचना यह देखने के लिए होता है कि सिस्टम में एक विशेष फोल्डर मौजूद है या नहीं। प्रोग्रामर्स यह चेक करते हैं ताकि वे एरर हैंडलिंग कर सकें, फाइल्स क्रिएट या मोडिफाई कर सकें, या डायरेक्टरीज में ऑपरेशन्स करने से पहले सुनिश्चित हो सकें कि डायरेक्टरी उपलब्ध है।

## How to: (किस प्रकार:)
Clojure में डायरेक्टरी के अस्तित्व की जांच `java.io.File` क्लास और `clojure.java.io/file` फंक्शन के साथ की जा सकती है। यहाँ एक सरल उदाहरण है:

```clojure
(defn directory-exists? [path]
  (let [dir (clojure.java.io/file path)]
    (and (.exists dir) (.isDirectory dir))))

(println (directory-exists? "/tmp")) ;; जांचता है कि /tmp डायरेक्टरी मौजूद है या नहीं
(println (directory-exists? "/thisdoesnotexist")) ;; अस्तित्वहीन डायरेक्टरी की जांच
```
संभावित आउटपुट हो सकता है:
```
true
false
```

## Deep Dive (गहराई से जानकारी):
पहले सॉफ्टवेयर प्रोग्राम्स में, डायरेक्टरी की मौजूदगी का पता करने का मैकेनिज़्म बहुत प्राथमिक था या कई बार मौजूद भी नहीं था। समय के साथ, फाइल सिस्टम इंटरैक्शन APIs ने विकास किया और इसे आसान बनाया। Clojure, जावा प्लेटफार्म पर चलने वाली एक डायनैमिक प्रोग्रामिंग भाषा होने के नाते, जावा क्लासेज़ और लाइब्रेरीज को इंटरैक्ट कर सकती है। इसलिए, जब हम डायरेक्टरी की जांच करते हैं तो हम वास्तव में जावा की `File` क्लास का उपयोग कर रहे होते हैं। 

वैकल्पिक तरीकों में हम `nio.file.Files` और `nio.file.Paths` क्लासेज का उपयोग कर सकते हैं, जो अधिक समकालीन हैं और बेहतर परफॉर्मेंस तथा अधिक सुविधाएँ प्रदान करते हैं। लेकिन क्लोजर का साधारण और फंक्शनल दृष्टिकोण इसे सीखने और लागू करने में आसान बनाता है।

## See Also (और भी देखें):
- Java `File` Class Documentation: [Java Platform SE 8](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- Clojure `java.io` Namespace: [Clojure Docs](https://clojuredocs.org/clojure.java.io)
- Java NIO Files Tutorial: [Java NIO Files](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
