---
title:                "Clojure: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

"# यामल क्यों?\n
यामल को स्ट्रक्चर्ड डेटा फॉर्मैट के रूप में उपयोग किया जाता है जो अनुपात, सूची, और शामिल विवरणों को निर्दिष्ट करने के लिए डिजाइन किया गया है। यह जनरल फॉर्मेट संरचना के साथ फाइलों को सुरक्षित रूप से संग्रहीत और साझा करने का एक अच्छा तरीका है।

## कैसे करें?

```Clojure
;; यामल फाइल पढ़ें
(def yaml-data (slurp "data.yaml"))

;; यामल डेटा पार्स करें
(require '[clj-yaml.core :as yaml])
(yaml/parse-string yaml-data)

;; यामल फाइल लिखें
(def yaml-data {:name "John" :age 25 :hobbies ["reading" "writing"]})
(spit "data.yaml" (yaml/generate-string yaml-data))
```
सॉर्स: [Clojure स्क्रिप्टों को यामल में बदलें](https://yobriefca.se/blog/2014/05/19/converting-clojure-scripts-to-yaml/) 

## गहराई में जाएं

यामल तकनीकी फॉर्मेट क्यों है और कैसे यह डेटा संरचित करेगा, इसे समझने के लिए "मॉपिंग द्वारा उत्पन्नता" प्रक्रिया जानना उपयोगी हो सकता है। यामल नोटेशन में, कुंजी स्ट्रिंग से चिन्ह हटाकर फाइल को समान संरचना सामग्री से जाने जाते हैं। यह अनुकूलन जानकार को इष्ट स्ट्रक्चर को नियंत्रित करने का एक पूर्वनिर्धारित तथा स्पष्ट तरीका देता है।

सॉर्स: [यामल व्याख्या](https://yaml.org/spec/1.2/spec.html) 

# संबंधित लिंक
- [यामल ऑनलाइन संग्रहक](https://yaml-online-parser.firebaseapp.com/)
- [यामल को सीखे](https://learnxinyminutes.com/docs/hi-in/yaml-hi/) 
- [यामल बेस कोडीटोरियल](https://www.youtube.com/watch?v=cdLNKUoMc6c)

"