---
title:    "Clojure: दो तारीखों की तुलना करना"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# क्यों

दो तारीखों को तुलना करने का उद्देश्य हो सकता है कि सुनिश्चित किया जाए कि वे आपकी आवश्यकताओं को कवर कर सकते हैं।

# कैसे करें

```Clojure
(import java.time.LocalDate)

(def today (LocalDate/now))
(def tomorrow (LocalDate/now))
(def result (.compareTo today tomorrow))

(println result) ;लगता है: -1
```

# गहराई में

दो तारीखों को तुलना करने के बारे में अधिक जानने के लिए आप हजारों विभिन्न तारीख तुलना फ़ंक्शन्स (functions) का उपयोग कर सकते हैं, जो Java 8 में शामिल हैं। इनमें से कुछ शामिल हैं: .compare(), .equals(), और .compareTo()। इसमें से हर एक का उपयोग अपने तरीके से होता है।

# इससे जुड़े लिंक्स

[जावा टाइम API डॉक्यूमेंटेशन](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)

[Clojure में जावा रफ़ेरेंस](https://clojure.org/reference/java_interop)

[Clojure में तारीख और समय भंडारण करना](https://clojure.org/guides/date_time)