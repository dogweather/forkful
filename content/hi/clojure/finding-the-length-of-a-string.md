---
title:                "Clojure: स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# क्यों:

शब्द स्तंभ की लंबाई निकालना हमारी सभी भाषाओं में उपयोगी और महत्वपूर्ण है। यह एक प्रोग्रामर के लिए अनिवार्य कौशल है जो संख्यात्मक डेटा का उपयोग करना चाहता है।

# कैसे करें:

```Clojure
(defn length [str]
  (count str))

(print (length "नमस्ते")) ;; आउटपुट: 6
```

```Clojure
(defn length [coll]
  (reduce (fn [accum item] (+ 1 accum)) 0 coll))

(print (length [1 2 3 4 5])) ;; आउटपुट: 5
```

# गहराई में जाएं:

शब्द स्तंभ की लंबाई निकालने के पीछे एक साधारण सामान्य प्रक्रिया है। लेकिन यह काम किसी भी स्थिति में आराम से किया जा सकता है। आप समूह की लंबाई निकालने के अलावा स्ट्रिंग में वर्णों की संख्या भी निकाल सकते हैं। आपको यह भी कह सकते हैं कि इस कार्य में सही सरल तरीका भी है।

# देखें भी:

- [Clojure भाषा सीखें](https://github.com/vijaysharm/learn-clojure)
- [Clojure के सूत्र](https://devhints.io/clojure)
- [Clojure से परिचित होने के लिए पांच महत्वपूर्ण कारण](https://www.thoughtworks.com/insights/blog/thinking-clojure-five-reasons-i-became-chalangier-in-workday)