---
title:                "वर्तमान तारीख प्राप्त करना"
aliases: - /hi/clojure/getting-the-current-date.md
date:                  2024-02-03T19:10:02.466773-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामिंग में वर्तमान तारीख प्राप्त करना कई कारणों से महत्वपूर्ण होता है, जिसमें लॉगिंग, समय-चिह्न घटनाओं, और कार्यों को अनुसूचित करना शामिल हैं। Clojure में, जो JVM पर एक Lisp उपभाषा है, यह कार्य Java अंतःक्रियाशीलता क्षमताओं का लाभ उठाते हुए, अमीर Java Date-Time API तक सरल पहुँच की अनुमति देता है।

## कैसे:

### Java अंतःक्रियाशीलता का उपयोग करके
Clojure की Java के साथ सहज अंतःक्रियाशीलता आपको Java Date-Time API से सीधे जुड़ने की अनुमति देती है। यहाँ वर्तमान तारीख कैसे प्राप्त करें:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; नमूना आउटपुट
(get-current-date) ; "2023-04-15"
```

### clj-time लाइब्रेरी का उपयोग करके
एक Clojure विशिष्ट समाधान के लिए, आप `clj-time` लाइब्रेरी का विकल्प चुन सकते हैं, जो Joda-Time के चारों ओर एक लपेट है, हालांकि अधिकांश नए प्रोजेक्ट्स के लिए, बिल्ट-इन Java 8 Date-Time API की सिफारिश की जाती है। हालाँकि, यदि आप `clj-time` पसंद करते हैं या आवश्यकता होती है:

सबसे पहले, अपनी प्रोजेक्ट निर्भरताओं में `clj-time` जोड़ें। अपनी `project.clj` में शामिल करें:

```clojure
[clj-time "0.15.2"]
```

फिर, वर्तमान तारीख प्राप्त करने के लिए इसका उपयोग करें:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; नमूना आउटपुट
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

दोनों विधियाँ Clojure में वर्तमान तारीख प्राप्त करने के लिए त्वरित, प्रभावी तरीके प्रदान करती हैं, अंतर्निहित Java प्लेटफॉर्म की शक्ति का लाभ उठाते हुए या Clojure-विशिष्ट लाइब्रेरी की सुविधा का उपयोग करते हैं।
