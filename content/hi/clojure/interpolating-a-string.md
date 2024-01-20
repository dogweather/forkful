---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग इंटरपोलेशन मतलब किसी स्ट्रिंग के भीतर वेरियेबल का मूल्य सम्मिलित करना। प्रोग्रामर्स इसे कोड को पठनीय और संवादात्मक बनाने के लिए करते हैं।

## कैसे करें:

Clojure में, आप `str` फ़ंक्शन का उपयोग करके स्ट्रिंग इंटरपोलेशन कर सकते हैं। नीचे उदाहरण देखें:

```Clojure
(def my-name "Raju")
(def my-message (str "नमस्ते, " my-name))
```
ऊपर के कोड का आउटपुट ऐसा होगा:
```
नमस्ते, Raju
```

## गहराई में:

1. ऐतिहासिक संदर्भ: Clojure का पहला संस्करण 2007 में आया था और तब से यह स्ट्रिंग इंटरपोलेशन का समर्थन करता आ रहा है।
2. वैकल्पिक: क्लोज़र में, आप `format` फ़ंक्शन का भी उपयोग कर सकते हैं, लेकिन `str` फ़ंक्शन का उपयोग करना अधिक सीधा होता है।
3. कार्यान्वयन विवरण: `str` फ़ंक्शन Clojure के आंतर्निक आरक्षित शब्दों में से एक है और इसे स्थानांतरित करने के लिए `StringBuilder` का उपयोग करता है।

## भी देखें:

और अधिक जानने के लिए, आप निम्नलिखित स्रोतों पर जा सकते हैं:
1. Clojure के आधिकारिक पुस्तकालय [str function](https://clojuredocs.org/clojure.core/str)
2. Clojure में स्ट्रिंग संभंधी [guide](https://clojure.org/guides/learn/strings)
3. [Format function](https://clojuredocs.org/clojure.core/format) के उपयोग के बारे में पढ़ें।