---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:35:11.890207-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग्स को जोड़ना यानी एक स्ट्रिंग के आखिर में दूसरी स्ट्रिंग लगाना। प्रोग्रामर्स यह इसलिए करते हैं कि इससे वे आसानी से विभिन्न टेक्स्ट संदेशों को संयोजित कर सकते हैं।

## How to: (कैसे करें:)
Clojure में स्ट्रिंग्स जोड़ने के लिए `str` फंक्शन का इस्तेमाल किया जाता है:

```Clojure
;; सिंपल स्ट्रिंग्स जोड़ना
(str "नमस्ते " "दुनिया!")
;; Output: "नमस्ते दुनिया!"

;; वेरिएबल्स के साथ स्ट्रिंग्स जोड़ना
(let [नाम "रोहन"]
  (str "नमस्ते, " नाम "!"))
;; Output: "नमस्ते, रोहन!"

;; संख्याओं और स्ट्रिंग्स को एक साथ जोड़ना
(str "आपके पास " 5 " संदेश हैं।")
;; Output: "आपके पास 5 संदेश हैं।"
```

## Deep Dive (गहराई में जानकारी)
Concatenation, यानी कन्केटनेशन, नयी तो नहीं हैं। यह संकलनन की प्रक्रिया का हिस्सा है जो शुरू से ही प्रोग्रामिंग में महत्वपूर्ण रहा है। Clojure का `str`फंक्शन दूसरी प्रोग्रामिंग भाषाओं के `concat` या `+` ऑपरेटर्स के समान है। परफॉरमेंस के लिहाज़ से, Clojure में स्ट्रिंग्स जोड़ते वक्त इम्म्यूटेबिलिटी का ध्यान रखा जाता है; हर `str` ऑपरेशन एक नयी स्ट्रिंग बनाता है। जब बहुत सारी स्ट्रिंग्स जोड़नी होती हैं, तो अन्य तकनिकी जैसे `StringBuilder` या `clojure.core/str-cat` का इस्तेमाल परफॉरमेंस में सुधार के लिए होता है।

## See Also (और भी देखें)
- Clojure `str` function: [Clojure `str`](https://clojuredocs.org/clojure.core/str)
- Text processing in Clojure: [Clojure Text Processing](https://www.braveclojure.com/clojure-for-the-brave-and-true/#Text_Processing)
