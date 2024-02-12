---
title:                "JSON के साथ काम करना"
aliases:
- /hi/clojure/working-with-json/
date:                  2024-02-03T19:23:06.543499-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
क्लोज़र में JSON (जावास्क्रिप्ट ऑब्जेक्ट नोटेशन) के साथ काम करना क्लोज़र डेटा संरचनाओं (मैप्स, वेक्टर्स) में JSON स्ट्रिंग्स को पार्स करने और इसके विपरीत करने की प्रक्रिया शामिल है। यह कार्य वेब सेवाओं, APIs, और ऐसे अनुप्रयोगों के लिए मौलिक है जिन्हें संरचित, पाठ-आधारित प्रारूप में डेटा संचारित करने की आवश्यकता होती है क्योंकि JSON विभिन्न प्रोग्रामिंग पर्यावरणों में सर्वव्यापी रूप से पहचाना जाता है और समर्थित होता है।

## कैसे करें:
क्लोज़र JSON के साथ काम करने के लिए निर्मित-इन फ़ंक्शन्स को शामिल नहीं करता, इसलिए आप आमतौर पर तृतीय-पक्ष लाइब्रेरियों का उपयोग करेंगे। `cheshire` और `jsonista` उनके उपयोग की आसानी और प्रदर्शन के कारण लोकप्रिय विकल्प हैं।

### Cheshire का उपयोग करना
पहले, `project.clj` में अपनी प्रोजेक्ट निर्भरताओं में Cheshire जोड़ें:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

एक JSON स्ट्रिंग को क्लोज़र मैप में पार्स करने और मैप को JSON स्ट्रिंग में बदलने के लिए:

```clj
(require '[cheshire.core :as json])

;; पार्स JSON स्ट्रिंग को क्लोज़र मैप में
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; क्लोज़र मैप को JSON स्ट्रिंग में बदलें
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Jsonista का उपयोग करना
अपनी परियोजना `project.clj` में Jsonista जोड़ें:
```clj
[jsonista "0.3.2"]
```

Jsonista के साथ समान क्रियाएं:

```clj
(require '[jsonista.core :as j])

;; पार्स JSON स्ट्रिंग को क्लोज़र में
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; क्लोज़र मैप को JSON स्ट्रिंग में बदलें
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

दोनों पुस्तकालयों में, आपके पास अधिक जटिल डेटा संरचनाओं को एन्कोड और डिकोड करने का विकल्प है, और सीरियलाइज़ेशन और डिसीरियलाइज़ेशन प्रक्रियाओं को अनुकूलित करने के लिए अतिरिक्त फ़ंक्शन और पैरामीटर हैं। अधिकांश अनुप्रयोगों के लिए, दिखाई गई कार्यक्षमता क्लौजर अनुप्रयोगों में JSON के साथ काम करने के लिए एक मज़बूत आधार प्रदान करती है।
