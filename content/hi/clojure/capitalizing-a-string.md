---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
एक स्ट्रिंग को कैपिटलाइज़ करना मतलब है हर शब्द के पहले अक्षर को बड़े अक्षर (कैपिटल लेटर) में बदलना। प्रोग्रामर्स ऐसा सामान्यत: यूज़र इंटरफेस और डॉक्युमेंटेशन में शीर्षक और नामों को उचित रूप से प्रदर्शित करने के लिए करते हैं।

## How to: (कैसे करें:)
Clojure में स्ट्रिंग को कैपिटलाइज़ करने के लिए हम `clojure.string/capitalize` फंक्शन का उपयोग करते हैं। ध्यान दें, यह हर शब्द के पहले अक्षर को कैपिटलाइज़ नहीं करता, केवल स्ट्रिंग के पहले अक्षर को बड़ा करता है। हर शब्द को कैपिटलाइज़ करने के लिए आपको अतिरिक्त कोड लिखना पड़ेगा।

```Clojure
(require '[clojure.string :as str])

;; पहले अक्षर को कैपिटलाइज़ करना:
(str/capitalize "clojure मजेदार है.")
;; Output: "Clojure मजेदार है."

;; हर शब्द को कैपिटलाइज़ करने के लिए:
(defn capitalize-words [s]
  (->> s
       (str/split #"\s")
       (map str/capitalize)
       (str/join " ")))

(capitalize-words "clojure मजेदार है.")
;; Output: "Clojure मजेदार है."
```

## Deep Dive (गहराई में जानकारी)
स्ट्रिंग कैपिटलाइजेशन प्रोग्रामिंग भाषाओं में एक सामान्य फीचर है। `clojure.string/capitalize` फंक्शन Clojure 1.3 के रिलीज में शामिल किया गया था। हालांकि यह केवल पहले अक्षर को बड़ा करता है, कई बार हमें पूरे टाइटल को कैपिटलाइज़ करने की जरूरत पड़ती है। इस स्थिति में, हमें मैप और ज्वाइन का उपयोग करके अपना कस्टम फंक्शन बनाना पड़ता है। इससे हमें Clojure के फंक्शनल प्रोग्रामिंग प्रावधानों की शक्ति का भी पता चलता है।

विकल्पों की बात करें तो, Apache Commons Lang library जैसी कुछ जावा लाइब्रेरीज में भी कैपिटलाइजेशन फंक्शंस उपलब्ध हैं जिन्हें Clojure से उपयोग किया जा सकता है।

## See Also (संबंधित स्रोत)
- Clojure Documentation for `clojure.string`: [Official Docs](https://clojuredocs.org/clojure.string/capitalize)
- Clojure from the ground up: strings: [Guide](https://aphyr.com/posts/305-clojure-from-the-ground-up-strings)
- Java String Utilities in Clojure - Apache Commons Lang: [Commons Lang](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
