---
title:                "स्ट्रिंग इंटरपोलेशन"
date:                  2024-01-20T17:50:38.283930-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग इंटरपोलेशन से आशय वेरिएबल्स और एक्सप्रेशन्स को स्ट्रिंग के अंदर सीधे डालने से है. ये तकनीक प्रोग्रामर्स को कोड को और पठनीय और मेंटेन करने में आसान बनाती है.

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