---
title:                "Clojure: स्ट्रिंग को प्रथम अक्षर से लिखना"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप Clojure में नए हैं, तो आपने शायद देखा होगा कि कई कोडिंग प्रोजेक्ट में एक शब्द को शुद्ध कपिटलाइज किया जाता है। यह कई वयोग कोतिन शब्दों की पहचान करने में मदद कर सकता है और देखने में भी अच्छा लगता है। इस लेख में हम जानेंगे कि Clojure में स्ट्रिंग कैपिटलाइज कैसे करें।

## कैसे

```Clojure
;; एक कपिटलाइज्ड स्ट्रिंग बनाने के लिए, uppercase फ़ंक्शन का उपयोग करें
(println (uppercase "hello world")) ;; हेल्लो वर्ल्ड
```

आप देख सकते हैं कि एक सादा से "hello world" स्ट्रिंग को उपर्केस में बदलकर हमने कैपिटलाइज किया है। इसके अलावा, आप lowercase, capitalize या titleize भी उपयोग कर सकते हैं जो स्ट्रिंग को अनुसार बदलते हैं।

## गहराई में

Clojure में कैपिटलाइज करने की दृष्टि से यह सबसे थोड़ा समस्या है कि यह अक्षरों की उत्पत्ति के साथ अलग-अलग टाइम्स निर्धारित करता है कि वे शब्द को कैसे कैपिटलाइज करें। इसका मतलब है कि यदि आप "iPhone" या "Walmart" स्ट्रिंग को कैपिटलाइज करते हैं, तो आपको उसे "Iphone" और "Walmart" में बदलना होगा। इसके लिए सबसे अच्छा रास्ता config फ़ंक्शन का उपयोग करना है जो आपको अलग-अलग capitalization के लिए मानदंड निर्दिष्ट करता है।

## देखें अभी
- [Clojure strings tutorial](https://clojure.org/guides/learn/syntax#_strings)
- [Clojure string functions](http://clojure-doc.org/articles/language/strings.html)
- [Configuring capitalization in Clojure](https://lambdaisland.com/blog/2020-01-14-configuring-capitalization-in-clojure)