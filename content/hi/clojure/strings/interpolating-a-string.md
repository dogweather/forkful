---
date: 2024-01-20 17:50:38.283930-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) ."
lastmod: '2024-03-13T22:44:51.639217-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to: (कैसे करें)
```Clojure
;; स्ट्रिंग इंटरपोलेशन का उपयोग कैसे करें

;; clojure.core/str का उपयोग करके
(def name "विश्वजीत")
(def message (str "Hello, " name "! कैसे हैं आप?"))
(println message) ; आउटपुट: Hello, विश्वजीत! कैसे हैं आप?

;; format फंक्शन का उपयोग करके
(def message (format "Hello, %s! कैसे हैं आप?" name))
(println message) ; आउटपुट: Hello, विश्वजीत! कैसे हैं आप?
```

## Deep Dive (गहराई से जानकारी)
स्ट्रिंग इंटरपोलेशन का विचार नया नहीं है; यह कई प्रोग्रामिंग भाषाओं में सदियों से उपयोग हो रहा है. इसके विपरीत, Clojure में अन्य भाषाओं की तरह बिल्ट-इन स्ट्रिंग इंटरपोलेशन नहीं होता है. हम clojure.core/str या format जैसे फंक्शन्स का उपयोग करके इसे हासिल कर सकते हैं.

Clojure में एक और विकल्प `clojure.pprint/cl-format` होता है, जो अधिक जटिल फॉर्मेटिंग ऑपरेशन्स को सपोर्ट करता है, लेकिन यह शुरुआती के लिए कठिन हो सकता है.

कई क्लोजर लाइब्रेरीज जैसे कि `hiccup` वेब टेम्प्लेटिंग में स्ट्रिंग इंटरपोलेशन का उपयोग करती हैं.

## See Also (और देखें)
- Clojure `str` documentation: [str | Clojure Docs](https://clojuredocs.org/clojure.core/str)
- Clojure `format` documentation: [format | Clojure Docs](https://clojuredocs.org/clojure.core/format)
- `clojure.pprint/cl-format` for advanced formatting: [cl-format | Clojure Pretty Print](https://clojuredocs.org/clojure.pprint/cl-format)
