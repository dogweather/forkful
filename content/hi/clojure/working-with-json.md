---
title:                "जेसोन के साथ काम करना"
html_title:           "Clojure: जेसोन के साथ काम करना"
simple_title:         "जेसोन के साथ काम करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

आज के समय में, डाटा विन्यास प्राथमिकताओं में से एक है। जेसन (JSON) एक लोकप्रिय डेटा फॉर्मेट है जिसका उपयोग सभी तरह के डाटा को संगठित करने के लिए किया जाता है, चाहे वह वेब सामग्री हो या मोबाइल ऍप्स। ऐसे में, जेसन (JSON) के साथ काम करना एक महत्वपूर्ण कौशल है जो हर डेवलपर को सीखना चाहिए।

## कैसे करें

जेसन (JSON) का उपयोग करने के लिए, हमारे पास clojure.data.json नामक लाइब्रेरी है। इसे इम्पोर्ट करने के लिए, हम `(:require [clojure.data.json :as json])` का उपयोग कर सकते हैं।

```Clojure
(require '[clojure.data.json :as json])

(def sample-data {:name "John" :age 30 :occupation "Software Engineer"})
```

आप अपने डेटा को map के रूप में डिक्लेयर कर सकते हैं और `json/write` फ़ंक्शन का उपयोग करके जेसन (JSON) फॉर्मेट में उसे लिख सकते हैं।

```Clojure
(json/write sample-data) ;; outputs: "{\"name\":\"John\",\"age\":30,\"occupation\":\"Software Engineer\"}"
```

अगर हमें जेसन (JSON) फ़ॉर्मेट से अपने डेटा को पार्स करना हो, तो हम `json/parse-string` फ़ंक्शन का उपयोग करके डेटा को Clojure डेटा टाइप में लोड कर सकते हैं।

```Clojure
(json/parse-string "{\"name\":\"John\",\"age\":30,\"occupation\":\"Software Engineer\"}") ;; returns: {:name "John" :age 30 :occupation "Software Engineer"}
```

## डीप डाइव

जेसन (JSON) फ़ॉर्मेट में डेटा को संगठित करना काफी आसान है और सामान्य रूप से उपयोग किया जाता है। हालांकि, जानना कि कैसे `json/write` और `json/parse-string` फ़ंक्शन काम करते हैं एक बेहतरीन गहराई हो सकती है। इसलिए, आप Clojure डिक्शनरी के माध्यम से इन फ़ंक्शन की डॉक्यूमें