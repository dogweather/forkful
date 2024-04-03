---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:29.782610-07:00
description: "\u0915\u0948\u0938\u0947: Clojure \u092E\u0947\u0902 YAML \u0915\u0947\
  \ \u0932\u093F\u090F \u092C\u0928\u093E\u0908 \u0917\u0908 \u0938\u0939\u093E\u092F\
  \u0924\u093E \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\u0940\u0902 \u0939\u0948\
  , \u0932\u0947\u0915\u093F\u0928 \u0906\u092A `clj-yaml` \u091C\u0948\u0938\u0940\
  \ \u0924\u0940\u0938\u0930\u0947 \u092A\u0915\u094D\u0937 \u0915\u0940 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0915\u0947 YAML \u0921\u0947\u091F\u093E \u0915\u093E\
  \ \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0914\u0930\u2026"
lastmod: '2024-03-13T22:44:51.698321-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u092E\u0947\u0902 YAML \u0915\u0947 \u0932\u093F\u090F \u092C\u0928\
  \u093E\u0908 \u0917\u0908 \u0938\u0939\u093E\u092F\u0924\u093E \u0936\u093E\u092E\
  \u093F\u0932 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\u0915\u093F\u0928\
  \ \u0906\u092A `clj-yaml` \u091C\u0948\u0938\u0940 \u0924\u0940\u0938\u0930\u0947\
  \ \u092A\u0915\u094D\u0937 \u0915\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940\u091C \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 YAML \u0921\u0947\u091F\u093E \u0915\u093E \u092A\u093E\u0930\u094D\u0938\
  \u093F\u0902\u0917 \u0914\u0930 \u091C\u0947\u0928\u0930\u0947\u0936\u0928 \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0938\u092C\u0938\u0947\
  \ \u092A\u0939\u0932\u0947, \u0905\u092A\u0928\u0940 \u092A\u0930\u093F\u092F\u094B\
  \u091C\u0928\u093E \u0928\u093F\u0930\u094D\u092D\u0930\u0924\u093E\u0913\u0902\
  \ \u092E\u0947\u0902 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u091C\
  \u094B\u0921\u093C\u0947\u0902."
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 41
---

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
