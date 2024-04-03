---
date: 2024-01-20 17:33:58.113249-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: (How to) Clojure\
  \ \u092E\u0947\u0902 \u0924\u093F\u0925\u093F\u092F\u094B\u0902 \u0915\u093E \u0924\
  \u0941\u0932\u0928\u093E clj-time \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\
  \u0940 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:51.684802-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u092E\u0947\u0902 \u0924\u093F\u0925\u093F\u092F\u094B\u0902 \u0915\
  \u093E \u0924\u0941\u0932\u0928\u093E clj-time \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902."
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

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
