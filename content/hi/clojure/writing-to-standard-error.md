---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
simple_title:         "मानक त्रुटि में लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्टैंडर्ड एरर में लिखने का मतलब है एरर मैसेज को प्रोग्राम के सामान्य आउटपुट से अलग रखना। प्रोग्रामर्स यह करते हैं ताकि उपयोगकर्ता और अन्य प्रोग्राम्स गलतियों को आसानी से पहचान सकें।

## How to: (कैसे करें:)
```Clojure
; एक सामान्य प्रिंट स्टेटमेंट
(println "यह मैसेज स्टैंडर्ड आउटपुट पर जाएगा")

; स्टैंडर्ड एरर पर मैसेज भेजने के लिए
(.write *err* "यह एरर मैसेज स्टैंडर्ड एरर पर जाएगा\n")

; flush का यूज तुरंत आउटपुट दिखाने के लिए
(.flush *err*)
```
सैंपल आउटपुट:
```
यह मैसेज स्टैंडर्ड आउटपुट पर जाएगा
यह एरर मैसेज स्टैंडर्ड एरर पर जाएगा
```

## Deep Dive (गहराई में जानकारी)
स्टैंडर्ड एरर (stderr) एक स्टैंडर्ड आउटपुट स्ट्रीम की तरह होता है, जो यूनिक्स जैसे सिस्टम्स में दशकों से मौजूद है। इसका मुख्य उद्देश्य एरर मैसेजेस और डायग्नोस्टिक्स को स्टैंडर्ड आउटपुट से अलग करना है। जबकि `*err*` का यूज करके स्टैंडर्ड एरर में लिखा जाता है, कुछ विकल्प भी हैं जैसे कि लॉगिंग लाइब्रेरीज का उपयोग करना। 

## See Also (और जानकारी)
- [Clojure Docs - java.io.Writer](https://clojuredocs.org/clojure.java.io/writer)
- [Practical Common Lisp - Handling Errors](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html)
- [The Java Tutorials - I/O Streams](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
