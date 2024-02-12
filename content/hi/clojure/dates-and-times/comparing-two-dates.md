---
title:                "दो तारीखों की तुलना"
aliases:
- /hi/clojure/comparing-two-dates/
date:                  2024-01-20T17:33:58.113249-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
तिथियों की तुलना यानी दो तिथियाँ का अंतर निकालना। यह जानने के लिए किया जाता है कि दो घटनाएँ कितना समय अलग हैं। यह उन मामलों में उपयोगी है जैसे कि अवधि की गणना, उम्र निर्धारण या समय-सीमा जाँच।

## कैसे करें: (How to)
Clojure में तिथियों का तुलना clj-time लाइब्रेरी का उपयोग करके करते हैं:

```Clojure
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

; दो तिथियाँ बनाये
(def date1 (coerce/to-date-time "2023-01-01T12:00:00.000Z"))
(def date2 (coerce/to-date-time "2023-01-02T12:00:00.000Z"))

; अंतर निकालें
(def duration (time/interval date1 date2))

; अंतर को दिनों में प्राप्त करें
(time/in-days duration)
```

उपरोक्त कोड के सैंपल आउटपुट कुछ ऐसा होगा:
```
1
```

## गहन जानकारी (Deep Dive)
Clojure में तिथियों की तुलना का इतिहास JVM के जोडा-टाइम से जुड़ा हुआ है, जिसे बाद में java.time पैकेज ने ले लिया। clj-time लाइब्रेरी का उपयोग कर डेवलपर्स आसानी से तिथियों के अंतर को नाप सकते हैं। विकल्प के रूप में, java.time पैकेज को सीधे Clojure में भी इस्तेमाल किया जा सकता है। इंप्लिमेंटेशन की बात करें तो, प्रदर्शन और सटीकता के लिहाज़ से यह काफी भरोसेमंद है। तिथियों की तुलना जटिल हो सकती है क्योंकि टाइम जोन्स, डेलाइट सेविंग, और लीप वर्ष जैसे पहलुओं को संभालना पड़ता है।

## और भी जानें (See Also)
- clj-time GitHub repository: [clj-time GitHub](https://github.com/clj-time/clj-time)
- Clojure official documentation: [Clojure Docs](https://clojure.org/api/api)
- Java.time package guide: [Java.time guide](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
