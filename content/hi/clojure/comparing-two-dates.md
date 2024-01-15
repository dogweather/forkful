---
title:                "दो तारीखों की तुलना करना"
html_title:           "Clojure: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों
लोग दो तारीखों को तुलना करने में संलग्न होते हैं क्योंकि वे दो विशिष्ट वर्षों, महीनों, अथवा दिनों के बीच अंतरों को जानना चाहते हैं जो उनके चयनित समयक्रम को प्रभावित कर सकते हैं।

## कैसे करें
```Clojure
(import 'java.time.LocalDate)

(def today (LocalDate/now))
(def yesterday (LocalDate/of 2019 12 1))

(println today)
;;=> #<LocalDate 2020-01-01>
(println yesterday)
;;=> #<LocalDate 2019-12-01>

(def difference (.between yesterday today ChronoUnit/DAYS))

(println difference)
;;=> 31 
```

## गहराई में जाएं
तारीखों को तुलना करने के लिए, हम पुष्टि कर सकते हैं कि एक दिन को दूसरे से क्षेत्र के रूप में संदर्भित किया जा सकता है। यहां, हम ```ChronoUnit``` का उपयोग कर सकते हैं मतलब व्यंजनोत्पाद इकाई जैसे वर्षों और महीनों को तुलना करने के लिए। अतिरिक्त मैथमैटिकल ऑपरेटर्स और लॉजिकल ऑपरेटर्स का भी उपयोग किया जा सकता है जैसे कि ```+``` या ```>```।

## देखें भी
- [जावा 8 डेट और टाइम उद्धरण](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)
- [बेकोजी पुस्तक "क्लोजर में जावा अभिज्ञान की काश: राउंड 2" का अध्ययन करें](https://www.amazon.com/Clojure-Applied-Second-Benjamin-Van/dp/1680500821)