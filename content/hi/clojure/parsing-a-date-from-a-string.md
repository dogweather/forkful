---
title:                "एक स्ट्रिंग से तारीख को पार्स करना"
html_title:           "Clojure: एक स्ट्रिंग से तारीख को पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख को पार्स करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

पार्सिंग तारीख स्ट्रिंग से एक तारीख निकालने को कहते हैं। यह काम विशेषकर कंप्यूटर प्रोग्रामिंग में तारीखों को संसाधित करने के लिए किया जाता है।

## तरीका:

```Clojure
(require '[clojure.java-time :as time])

;; String तारीख को तारीख में परिवर्तित करने के लिए:
(time/local-date "2021-04-01")
;; => #java.time.LocalDate "2021-04-01"

;; परिवर्तित तारीख को फॉर्मेट करने के लिए:
(time/format (time/date-time 2021 4 1) "dd-MMM-yyyy")
;; => "01-Apr-2021"
```

## गहराई जाँच:

हिस्ट्री से पार्सिंग तारीख स्ट्रिंग को कैसे करें, अलग विकल्प और इसे कैसे लागू किया जाता है जैसे कि कॉपीलेवल प्रोग्रामिंग को अध्ययन करें।

## और देखें:

- [Date/Time Formatting](https://clojure.org/api/java.time)
- [Java's datetime tutorial](https://docs.oracle.com/javase/tutorial/datetime/)