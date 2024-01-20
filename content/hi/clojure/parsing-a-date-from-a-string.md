---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

धागे से एक तारीख की पाठ्यक्रमण क्या होती है? यह एक कार्य होता है जिसमें किसी धागे में संग्रहीत तारीख को मिलाने के लिए किसी तारीख के प्रारूप को मान्यता की जाती है। प्रोग्रामर इसे विशेष रूप से क्यों करते हैं? यह हमें डाटा को विशिष्ट तारीख प्रारूपों में पुन: स्थापित करने, मिलाने और बदलने की क्षमता देता है।

## कैसे:

Clojure में, हमें Java के java.time लाइब्रेरी के साथ काम करने के लिए clj-time लाइब्रेरी का उपयोग करना होगा।

```Clojure
(ns date-parsing.core
  (:require [clj-time.coerce :as c]
            [clj-time.format :as f]))

(defn parse-date [s] 
  (c/from-string (f/parse s)))
```

अगर हमें "2020-12-12" को parse करना हो तो, हमें केवल `parse-date` function को call करना होगा:

```Clojure
(parse-date "2020-12-12")
```

और यह हमें #inst "2020-12-12T00:00:00.000-00:00" ऑउटपुट देगा।

## गहराईवादी जानकारी:

1. इतिहासिक संदर्भ: Clojure को Java पर बनाया गया था, और डेटा को पार्स करने के हमारे लिए मुख्य पुस्तकालय हमें Java से ही मिला।

2. विकल्प: 'clj-time' के अलावा 'java.time' जैसे अन्य लाइब्रेरी भी है जिसका उपयोग किया जा सकता है। 

3. कार्यान्वयन विवरण: दी गई तारीख के पाषण का कार्य java.time लाइब्रेरी के द्वारा किया जाता है, और clj-time लाइब्रेरी बस इसे Clojure के लिए अधिक उपयोगी ढांचे में रूपांतरित करती है।

## और देखें:


2. [clj-time GitHub](https://github.com/clj-time/clj-time)

3. [Java Date और Time API (Java 8)](https://www.javatpoint.com/java-8-date-time-api)