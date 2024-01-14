---
title:                "Clojure: एक पाठ फ़ाइल पढ़ना"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

टेक्स्ट फ़ाइल को समझना और उसमे दिए गए डेटा का उपयोग करना कोडिंग में अहम भूमिका निभाता है। यह ब्लॉग पोस्ट हिंदी पाठकों के लिए Clojure प्रोग्रामिंग को समझने का अच्छा माध्यम है।

## कैसे करें

टेक्स्ट फ़ाइल को पढ़ने के लिए, हम `java.io` लाइब्रेरी का उपयोग कर सकते हैं।

```Clojure
(import java.io.FileReader)
(import java.io.BufferedReader)

(def file (FileReader. "sample.txt"))
(def reader (BufferedReader. file))

(loop [line (.readLine reader)]
  (when line
    (println line)
    (recur (.readLine reader))))

```

यहां `sample.txt` की जगह आप अपनी टेक्स्ट फ़ाइल के नाम का उपयोग कर सकते हैं। प्रत्येक पंक्ति `println` के माध्यम से कॉन्सोल पर प्रिंट होगी। अगर आप कोडिंग का अभ्यास करना चाहते हैं तो आप `recur` की जगह `map` भी प्रयोग कर सकते हैं जो आपको सभी पंक्तियों का तालिका बना कर देगा।

## गहराई में जाएं

टेक्स्ट फ़ाइल को पढ़ने के लिए कई तरीके हैं और कुछ लोगों के लिए यह कठिन भी हो सकता है। Clojure में `java.io` लाइब्रेरी के अलावा भी आपको `clojure.java.io` बिल्ट-इन फ़ंक्शन्स का उपयोग करने की सुविधा है। कुछ लोग खुद को आराम से `line-seq` का उपयोग करते हुए टेक्स्ट फ़ाइल को साधनाओं में विभाजित करने में पाते हैं।

## देखें भी

 [Clojure कोडिंग के लिए ये 5 कारगर टोटल्स](https://www.hackerearth.com/blog/programming/5-essential-clojure-libraries/) 
- इस लेख में हमने कुछ और भी लाइब्रेरी के बारे में बात की है जो आपको Clojure कोडिंग में ब