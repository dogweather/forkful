---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML एक ह्यूमन-रीडेबल डाटा सीरियलाइजेशन स्टैण्डर्ड है, जिसे आमतौर पर कॉन्फ़िग फाइलों और डेटा मॉडलिंग के लिए इस्तेमाल किया जाता है. प्रोग्रामर्स YAML का इस्तेमाल डाटा को सरल और पढ़ने योग्य फॉर्मेट में प्रस्तुत करने के लिए करते हैं.

## कैसे करें:

Clojure में YAML का प्रयोग बिना मुश्किल के कर सकते हैं. `clj-yaml` लाइब्रेरी का इस्तेमाल करके YAML फाइल को पढ़ा और लिखा जा सकता है.

```clojure
;; clj-yaml लाइब्रेरी को ऐड करना
(require '[clj-yaml.core :as yaml])

;; YAML डाटा को पढ़ना
(let [input-yaml (slurp "config.yaml")]
  (println (yaml/parse-string input-yaml)))

;; YAML फाइल में डाटा लिखना
(let [data {:name "अनुराग" :उम्र 25}]
  (spit "output.yaml" (yaml/generate-string data)))
```

इससे हमें config.yaml से पार्स किया गया डाटा मिलेगा और output.yaml में नया डाटा लिखा जाएगा.

## गहराई में जानकारी

YAML 2001 में आया था, JSON और XML का एक सरल रूप है. ये खासतौर पर पढ़ने में आसान होता है और डेटा संरचनाओं के साथ अच्छा काम करता है. Clojure में `clj-yaml` और `snakeyaml` जैसे बिब्लियोथिक इस्तेमाल किए जा सकते हैं. विकल्पों में EDN (Extensible Data Notation) है जो Clojure का ही एक पार्ट है. 

## अन्य स्रोत

आगे पढ़ने और रिफरेंस के लिए:

- YAML स्पेक्स: https://yaml.org/spec/
- `clj-yaml` GitHub पेज: https://github.com/clj-commons/clj-yaml
- `snakeyaml` डॉक्यु्मेण्टेशन: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation

YAML के बारे में और जाने और Clojure प्रोजेक्ट्स में इसका इस्तेमाल करने का प्रयास करें.
