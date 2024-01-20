---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:13:47.080778-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

वर्तमान तारीख प्राप्त करना कंप्यूटर सिस्टम का समय जानने की प्रक्रिया है। प्रोग्रामर इसे लॉगिंग, टाइमस्टैम्प, और अन्य समय-संवेदी कार्यवाहियों के लिए करते हैं।

## How to (कैसे करें):

Clojure में वर्तमान तारीख प्राप्त करने के लिए, java.util.Date लाइब्रेरी का उपयोग करें:

```Clojure
(import 'java.util.Date)

(defn current-date []
  (str (Date.)))

(println (current-date))
```

जब आप ऊपर दिए गए कोड को चलाएंगे, आपको एक आउटपुट मिलेगा जो वर्तमान तारीख और समय दिखाता है। 

सैम्पल आउटपुट यूँ होगा:

```
"Wed Apr 05 15:49:23 IST 2023"
```

## Deep Dive (गहराई में जानकारी):

Clojure एक लिस्प डायलेक्ट है जो JVM (Java Virtual Machine) पर चलता है। इसलिए, Java के क्लासेज को इसमें आसानी से इस्तेमाल किया जा सकता है। java.util.Date एक पुराना API है; नए कोड में java.time (JSR-310) के लिए ऑप्ट करना बेहतर होता है, जो दिनांक और समय प्रबंधन के लिए नए फीचर्स लाता है।

Clojure में जावा.time का इस्तेमाल कुछ ऐसे हो सकता है:

```Clojure
(import 'java.time.LocalDateTime)
(import 'java.time.format.DateTimeFormatter)

(defn current-date-time []
  (.format (LocalDateTime/now)
           (DateTimeFormatter/ofPattern "dd-MM-yyyy HH:mm:ss")))

(println (current-date-time))
```

इस तरह से आपको फॉर्मेटेड डेट और टाइम मिल जाएगा। इसके अतिरिक्त, Clojure में clj-time लाइब्रेरी भी है जो Joda-Time लाइब्रेरी पर आधारित है।

## See Also (और भी देखें):

- [Clojure Documentation](https://clojure.org/guides/getting_started)
- [java.util.Date JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [java.time package JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [clj-time GitHub Repository](https://github.com/clj-time/clj-time)