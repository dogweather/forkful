---
title:                "json के साथ काम करना"
html_title:           "Clojure: json के साथ काम करना"
simple_title:         "json के साथ काम करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## आप क्या और क्यों करें:
JSON काम करना क्या है और क्यों प्रोग्रामर उसे करते हैं, इसका एक सरल संक्षिप्त विवरण है। यह आपके डेटा को संरचित करने और इसे अन्य प्रोग्रामों, एप्लिकेशन और साइटों के साथ साझा करने के लिए एक स्टैंडर्ड फॉर्मेट है।

## कैसे करें:
```Clojure
(require '[cheshire.core :as json])

;; JSON स्ट्रिंग से मैप बनाएं
(json/parse-string "{\"name\": \"John\", \"age\": 30}")

;; मैप से JSON स्ट्रिंग बनाएं
(json/generate-string {:name "John", :age 30})
```

उपरोक्त कोड को अपने खुद के IDE में चलाएं और आप देखेंगे कि कैसे आप JSON स्ट्रिंग से मैप बना सकते हैं और मैप से फिर JSON स्ट्रिंग बना सकते हैं।

## गहराई में जाएं:
JSON 1999 में एक बहुत ही पॉपुलर डेटा फॉर्मेट के रूप में विकसित हुआ था। आज, यह डेटा एक्सचेंज के लिए एक श्रृंखला स्पेसिफिकेशन्स में इस्तेमाल किया जाता है, जो इसकी मानकीकृत फॉर्मेटिंग और ज्ञात डेटा प्रकारों को परिभाषित करता है। अन्य विकल्पों में से एक XML है, जिसे भी डेटा एक्सचेंज के लिए इस्तेमाल किया जाता है। JSON की तुलना में, XML को पढ़ना और लिखना कठिन और असंगत हो सकता है।

JSON को Clojure में प्रसंस्करण करने के लिए Cheshire नामक एक लोकप्रिय लाइब्रेरी है, जो हमने उपर दिखाए गए उदाहरण में इस्तेमाल किया है। हालांकि, आप अन्य विकल्पों की भी जांच कर सकते हैं, जैसे data.json या clj-json।

## आपको आगे पढ़ना चाहिए:
- [Clojure के लिए Cheshire Github repository](https://github.com/dakrone/cheshire): आप इस लाइब्रेरी को अपने प्रोजेक्ट में इस्तेमाल करना चाहते हैं, तो आप इस गाइड को देख सकते हैं।
- [Clojure data.json लाइब्रेरी दस्तावेज़](https://clojure.github.io/data.json/): अगर आप इस दौरान Clojure के साथ काम कर रहे हैं तो आप यह लाइब्रेरी भी देख सकते हैं।
- [Clojure clj-json लाइब्रेरी दस्तावेज़](https://github.com/myfreeweb/clj-json): यह दूसरा पॉपुलर Clojure जस्ट फॉरमेट लाइब्रेरी है और आप इसका भी इस्तेमाल कर सकते हैं।