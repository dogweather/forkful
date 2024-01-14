---
title:                "Clojure: तारीख को प्राप्त करना"
simple_title:         "तारीख को प्राप्त करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

आजकल कई लोगों के लिए समय और तारीख जानना बहुत महत्वपूर्ण है। हम लोगों के लिए अपूर्व है कि हम हर वक़्त जानते रहें कि वर्तमान समय क्या है। आप अपने कंप्यूटर पर अपनी पसंदीदा भाषा में भी तारीख और समय को देख सकते हैं।

## कैसे करें

```Clojure
(def current-date (java.util.Date.))

(println "वर्तमान तारीख है: "  (.toString current-date))

(def current-time (java.util.Date.))

(println "वर्तमान समय है: " (.toString current-time))
```

आउटपुट:

वर्तमान तारीख है: Fri Sep 10 14:45:31 IST 2021
वर्तमान समय है: Fri Sep 10 14:45:34 IST 2021

## गहराई में जाएँ

जब हम कोडिंग के बारे में सोचते हैं, तो हमारे दिमाग में यह प्रश्न आता है कि हमें वर्तमान तारीख कैसे प्राप्त करनी चाहिए। आमतौर पर, हम जावा के बारे में सोचते हैं और सोचते हैं कि हां, हम डेटा प्रोग्राम में वर्तमान तारीख प्राप्त कर सकते हैं। हालांकि, क्लोजर कोडिंग भाषा में, हम स्थानीय समय का भी प्रबंधन कर सकते हैं। इस लेख में, हमने वर्तमान तारीख कैसे प्राप्त करें और इसे अन्य भाषाओं में कैसे प्रदर्शित करें के बारे में सीखा।

## इससे सम्बंधित

* [वर्तमान तारीख और समय को प्रबंधित कैसे करें](https://www.tutorialspoint.com/clojure/primitive_time.htm)
* [Java के माध्यम से तारीख को प्राप्त करें](https://clojuredocs.org/clojure.java.api/java.util.Date)
* [क्लोजर में समय को प्रबंधित करने के बारे में और अधिक जानें](https://www.braveclojure.com/dates-and-times/)