---
title:                "YAML के साथ काम करना"
aliases:
- hi/clojure/working-with-yaml.md
date:                  2024-02-03T19:25:29.782610-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML, जिसका पूरा नाम "YAML Ain't Markup Language" है, एक मानव-पठनीय डेटा सीरियलाइजेशन प्रारूप है जिसका उपयोग कॉन्फ़िगरेशन फ़ाइलों और भिन्न डेटा संरचना वाली भाषाओं के बीच डेटा आदान-प्रदान के लिए किया जाता है। प्रोग्रामर YAML का उपयोग इसकी सादगी और पठनीयता के कारण करते हैं, जिससे यह एप्लिकेशन को कॉन्फ़िगर करने और पॉलीग्लॉट प्रोग्रामिंग वातावरणों में डेटा आदान-प्रदान की सुविधा देने के लिए एक आदर्श विकल्प बनता है।

## कैसे:

Clojure में YAML के लिए बनाई गई सहायता शामिल नहीं है, लेकिन आप `clj-yaml` जैसी तीसरे पक्ष की लाइब्रेरीज का उपयोग करके YAML डेटा का पार्सिंग और जेनरेशन कर सकते हैं। सबसे पहले, अपनी परियोजना निर्भरताओं में लाइब्रेरी जोड़ें:

```clojure
;; इसे अपनी project.clj निर्भरताओं में जोड़ें
[clj-yaml "0.7.0"]
```

यहाँ बताया गया है कि आप YAML का पार्सिंग कैसे कर सकते हैं और Clojure मैप्स को YAML में कैसे बदल सकते हैं।

### YAML पार्सिंग:

```clojure
(require '[clj-yaml.core :as yaml])

;; एक YAML स्ट्रिंग का पार्सिंग
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; आउटपुट:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Clojure से YAML जेनरेट करना:

```clojure
(require '[clj-yaml.core :as yaml])

;; एक Clojure मैप को YAML स्ट्रिंग में बदलना
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; आउटपुट:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

`clj-yaml` के साथ ये साधारण कार्यवाहियाँ Clojure एप्लिकेशंस में एकीकृत की जा सकती हैं ताकि कॉन्फ़िगरेशन फ़ाइलों को संभाला जा सके या उन सेवाओं या घटकों के साथ डेटा आदान-प्रदान की सुविधा हो सके जो YAML का उपयोग करते हैं।
