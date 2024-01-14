---
title:                "Clojure: भविष्य या भूतकाल में तिथि की गणना"
simple_title:         "भविष्य या भूतकाल में तिथि की गणना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

"## इसकी क्यों":

अगर आप वर्तमान तारीख से आगामी या पूर्व की तारीख को जानने या निर्धारित करने में रुचि रखते हैं, तो आप Clojure प्रोग्रामिंग भाषा का उपयोग करके इसे आसानी से कर सकते हैं। 

"## कैसे करें":

यदि आप Clojure भाषा को नया समझ रहे हैं, तो आपको `import` स्टेटमेंट के साथ `java.time.LocalDate` क्लास को इम्पोर्ट करना होगा। फिर, `LocalDate.now()` फ़ंक्शन का इस्तेमाल करके वर्तमान तारीख प्राप्त करें। उसके बाद, `plusDays()` फ़ंक्शन का उपयोग करके वर्तमान तारीख से जोड़ने या `minusDays()` फ़ंक्शन का उपयोग करके घटाने के लिए दिनों और तारीखों को पास करें। नीचे दिए गए कोड ब्लॉक में एक उदाहरण है:

```Clojure
(import java.time.LocalDate)
(def today (LocalDate/now))
(def future-date (.plusDays today 10))
(def past-date (.minusDays today 5))

(past-date) ; Output: #object[java.time.LocalDate 0x7561c5f5 "2022-02-25"]
(future-date) ; Output: #object[java.time.LocalDate 0x76c8b3dc "2022-03-12"]
```

"## गहराई में":

Clojure में तारीख की गणना करने के लिए कई तरीके हैं। उपरोक्त उदाहरण में, हम `plusDays()` और `minusDays()` फ़ंक्शन का इस्तेमाल करते हैं, लेकिन आप `plusMonths()` और `minusMonths()` फ़ंक्शन का इस्तेमाल करके महीने या `plusYears()` और `minusYears()` फ़ंक्शन का इस्तेमाल करके साल दोनों तारीखों को जोड़ने या घटाने के लिए भी कर सकते हैं। 

"## देखें":

- [JavaDocs में `LocalDate` की जानकारी](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [क्लोजर का ऑफिशियल वेबसाइट](https://clojure.org/)
- [Java और Clojure में तारीख की गणना के ब