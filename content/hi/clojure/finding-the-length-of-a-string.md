---
title:                "स्ट्रिंग की लंबाई खोजना"
html_title:           "Clojure: स्ट्रिंग की लंबाई खोजना"
simple_title:         "स्ट्रिंग की लंबाई खोजना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

### What & Why?:

"Finding the length of a string" स्ट्रिंग की लंबाई का पता लगाना है। यह programming में अहम तकनीक है क्योंकि यह स्ट्रिंग में कितने शब्द हैं अथवा उनका सारांश क्या है ये जानने के लिए प्रयुक्त होता है।

### How to:

आप Clojure में "count" फ़ंक्शन का प्रयोग करके आसानी से स्ट्रिंग की लंबाई पता लगा सकते हैं।

```
;; एक स्ट्रिंग डिफाइन करके
(def s "Hello, world!")

;; उस स्ट्रिंग की लंबाई की हिसाब से हमें उसके करक्टरों की संख्या मिल जाएगी
(drop 2 (count s))

;; परिणाम होगा 12
```

### Deep Dive:

प्राचीन काल में, स्ट्रिंग की शब्दों की संख्या के पता लगाने के लिए अनेक तरीके जैसे अंगुलियों की गिनती करने, से आ आविष्कार करते थे। लेकिन programming के विकास से साथ साथ, एक सिंपल और अधिक प्रभावशाली तकनीक इस समस्या को हल करती हैं, जिसका नाम है "count" फ़ंक्शन।

इसके अलावा, Clojure के अलावा ये तकनीक दूसरे programming languages में भी प्रयोग में आता है। हालाँकि, कुछ languages में इसका नाम और तरीका थोड़ा भिन्न हो सकता है। इसलिए, "count" फ़ंक्शन को सिर्फ़ Clojure के लिए ही सीमित नहीं समझना चाहिए।

### See Also:

अगर आप और भी Clojure में तकनीक सीखना चाहते हैं तो आप ये संसाधन देख सकते हैं:

- [Clojure दस्तावेज़ीकरण](https://clojure.org/documentation)
- [Clojure के विस्तृत समर्थन की सूची](https://clojure.org/community/resources)
- [और भी बहुत कुछ!](https://clojure.org/)