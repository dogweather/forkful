---
title:                "भविष्य या अतीत में तारीख की गणना"
date:                  2024-01-20T17:31:08.466849-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
तारीखों को भविष्य या अतीत में गणना करना एक प्रोग्रामिंग क्रिया है जिसमें हम वर्तमान तारीख से निश्चित समय के बाद या पहले की तारीख का पता लगाते हैं। यह भुगतान समय सीमा, इवेंट प्लानिंग, या डेटा रिटेंशन स्कीम्स जैसे प्रोग्राम्मिंग कार्यों के लिए आवश्यक है।

## कैसे करें? (How to:)
Clojure में डेट लाइब्रेरीज जैसे कि `clj-time` का इस्तेमाल करके हम आसानी से तारीखों की गणना कर सकते हैं।

```clojure
;; Clojure के लिए आवश्यक लाइब्रेरी जोड़ना
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])
(require '[clj-time.format :as format])

;; वर्तमान तारीख प्राप्त करना
(def now (time/now))

;; भविष्य में तीन दिन जोड़ना
(def three-days-from-now (time/plus now (time/days 3)))

;; अतीत में तीन दिन घटाना
(def three-days-ago (time/minus now (time/days 3)))

;; तारीखों को स्ट्रिंग्स में बदलना और प्रिंट करना
(println "अभी की तारीख:" (format/unparse (format/formatters :date-time-no-ms) coerce/to-date-time now))
(println "तीन दिनों के बाद की तारीख:" (format/unparse (format/formatters :date-time-no-ms) coerce/to-date-time three-days-from-now))
(println "तीन दिनों के पहले की तारीख:" (format/unparse (format/formatters :date-time-no-ms) coerce/to-date-time three-days-ago))
```

संभावित आउटपुट (वर्तमान तारीख के आधार पर बदलेगा):
```
अभी की तारीख: "2023-04-07T12:34:56.789Z"
तीन दिनों के बाद की तारीख: "2023-04-10T12:34:56.789Z"
तीन दिनों के पहले की तारीख: "2023-04-04T12:34:56.789Z"
```

## गहराई से जानकारी (Deep Dive)
`clj-time` लाइब्रेरी, Joda-Time पर आधारित है, जो Java के लिए एक मजबूत डेट-टाइम लाइब्रेरी है। Clojure, जो Java Virtual Machine (JVM) पर चलता है, सीधे इसका फायदा उठाता है। इसके अलावा, Java 8 और बाद में `java.time` पैकेज भी उपलब्ध है, जो समय और तारीख के साथ काम करने के लिए एक मजबूत फ्रेमवर्क प्रदान करता है।

समय और तारीख की गणना करते समय समय-क्षेत्र और डेलाइट सेविंग टाइम को भी ध्यान में रखना चाहिए, जो कभी-कभी जटिल हो सकते हैं। `clj-time` इन्हें संभालने में सहायक है।

दूसरे विकल्पों में `goog.date` शामिल है, जो ClojureScript के लिए दिनांक संबंधी कार्यों के साथ मदद करता है, जो कि Clojure का एक वेब-आधारित संस्करण है।

## देखें भी (See Also)
- clj-time GitHub repository: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Joda-Time documentation: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
- Java 8 java.time package: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- ClojureScript goog.date docs: [https://google.github.io/closure-library/api/goog.date.html](https://google.github.io/closure-library/api/goog.date.html)
