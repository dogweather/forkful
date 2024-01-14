---
title:                "Clojure: एक अस्थायी फ़ाइल बनाना"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपको कभी अपने कोड में टेम्परेरी फाइल की आवश्यकता हुई है? यह फाइल प्रोग्राम के ऊपर से निश्चित समय के लिए ही बनती है और बाद में अपना आप ही हट जाती है। इससे आपको अपने सिस्टम से घने फाइलों के साथ संपर्क करने की आवश्यकता नहीं होती है।

## कैसे करें

```Clojure
(require '[clojure.java.io :as io])

;; अहानित फाइल
(with-open [temp-file (io/temp-file "myapp" ".txt")]
  (println "आपका फाइल पथ है: " (.getAbsolutePath temp-file)))

;; नया डायरेक्टरी बनाएं और उसके भीतर अहानित फाइलों को सहेजें
(io/make-parents (io/temp-file "directory/myapp" ".txt"))
```

आउटपुट:

```
आपका फाइल पथ है: /tmp/myapp1234567890.txt
```

## गहराई में ढूंढें

जब आप `temp-file` फ़ंक्शन का उपयोग करते हैं, तो यह एक अहानित फाइल बनाने के लिए `java.io.File` का उपयोग करता है। आप अपने इनपुट और आउटपुट स्रोत पर संदेश भेजने या पाठ फ़ाइल को स्थानांतरित करने के लिए इस फाइल का उपयोग कर सकते हैं। इसके अलावा, आप अपने प्रोग्राम में अस्थायी डेटा को संग्रहीत करने के लिए भी इसका उपयोग कर सकते हैं।

## इसके अलावा देखें

[Clojure वेबसाइट](https://clojure.org/) पर फ़ाइल सिस्टम के बारे में अधिक जानें।

नमूना कोड और उदाहरणों के लिए [Clojure कोडबेस](https://clojuredocs.org/) देखें।

अपने कोड में दिक्कत आ रही है? [Clojurians कम्युनिटीज़](https://clojurians.slack.com/) में सहायता पाएं।

## देखें