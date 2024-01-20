---
title:                "JSON के साथ काम करना"
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON एक हल्का डेटा फॉर्मेट है, जिसे आसानी से पढ़ा और लिखा जा सकता है। Clojure प्रोग्रामर्स इसका इस्तेमाल डेटा आदान-प्रदान के लिए करते हैं, खासकर एपीआई कॉल्स में।

## How to: (कैसे करें:)
Clojure में JSON के साथ काम करने के लिए, `cheshire` पुस्तकालय का इस्तेमाल किया जाता है। यह Leiningen/Boot का उपयोग करके आसानी से जोड़ा जा सकता है:

```Clojure
;; प्रोजेक्ट की dependencies में `cheshire` जोड़ें
[cheshire "5.10.1"]
```

JSON को पार्स करना:

```Clojure
(require '[cheshire.core :as json])

;; JSON स्ट्रिंग को Clojure मैप में बदलें
(json/parse-string "{\"foo\": \"bar\", \"num\": 42}")
;; => {:foo "bar", :num 42}
```

Clojure मैप को JSON में बदलें:

```Clojure
(require '[cheshire.core :as json])

;; Clojure मैप को JSON स्ट्रिंग में बदलें
(json/generate-string {:foo "bar", :num 42})
;; => "{\"foo\":\"bar\",\"num\":42}"
```

## Deep Dive (गहराई में जानकारी)
JSON, JavaScript Object Notation का संक्षिप्त रूप है, जो 2000 के दशक में डगलस क्रॉकफोर्ड द्वारा विकसित किया गया था। Clojure में JSON के साथ काम करने के लिए `cheshire` के अलावा, कई विकल्प हैं, जैसे `clj-json` और `jsonista`। `cheshire` का इस्तेमाल इसकी गति और विशेषताओं के कारण किया जाता है। ये पुस्तकालय जैक्सन 2.x को इस्तेमाल करते हुए JVM पर JSON प्रोसेसिंग की पेशकश करते हैं।

## See Also (इसे भी देखें)
- Cheshire पुस्तकालय: [https://github.com/dakrone/cheshire](https://github.com/dakrone/cheshire)
- `clj-json` GitHub पेज: [https://github.com/mmcgrana/clj-json](https://github.com/mmcgrana/clj-json)
- `jsonista` GitHub पेज: [https://github.com/metosin/jsonista](https://github.com/metosin/jsonista)