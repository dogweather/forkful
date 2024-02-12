---
title:                "डायरेक्टरी मौजूद है या नहीं जाँचना"
date:                  2024-02-03T19:08:08.427891-07:00
model:                 gpt-4-0125-preview
simple_title:         "डायरेक्टरी मौजूद है या नहीं जाँचना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
क्लोज़र में एक डायरेक्टरी का अस्तित्व जांचना आपके क्लोज़र एप्लिकेशन के भीतर से एक फ़ाइल सिस्टम डायरेक्टरी की उपस्थिति की पुष्टि करता है। यह कार्य फ़ाइल ऑपरेशनों के लिए महत्वपूर्ण है, ताकि ऐसे डायरेक्टरीज़ के पढ़ने या लिखने में त्रुटियों से बचा जा सके जो वहां न हो, जिससे दृढ़ और त्रुटि-मुक्त कोड निष्पादन सुनिश्चित होता है।

## कैसे:
क्लोज़र, JVM भाषा होने के नाते, इस उद्देश्य के लिए Java का `java.io.File` क्लास का उपयोग कर सकती है। इस तरह के बुनियादी ऑपरेशन के लिए आपको किसी थर्ड-पार्टी लाइब्रेरी की आवश्यकता नहीं है। आप इसे कैसे कर सकते हैं इसका तरीका यह है:

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; उपयोग उदाहरण
(println (directory-exists? "/path/to/your/directory")) ;; सच या गलत
```

यह फंक्शन, `directory-exists?`, एक डायरेक्टरी पथ को एक स्ट्रिंग के रूप में लेता है और यदि डायरेक्टरी अस्तित्व में है तो `सच` और अन्यथा `गलत` वापस करता है। यह डायरेक्टरी पथ के साथ एक `File` ऑब्जेक्ट बनाकर और फिर इस ऑब्जेक्ट पर `.exists` मेथड को कॉल करके प्राप्त किया जाता है।

कच्चे Java इंटरऑप के अतिरिक्त, आप कुछ Java बॉयलरप्लेट को दूर करने वाली क्लोज़र लाइब्रेरीज का उपयोग कर सकते हैं। ऐसी ही एक लाइब्रेरी है `clojure.java.io`। हालाँकि, यदि एक डायरेक्टरी का अस्तित्व जांचने की बात हो, तो आप `File` क्लास का उपयोग करेंगे, पर आपको अन्य फ़ाइल ऑपरेशनों के लिए यह लाइब्रेरी उपयोगी लग सकती है। उदाहरण:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; उदाहरण उपयोग
(println (directory-exists?-clojure "/another/path/to/check")) ;; सच या गलत
```

यह संस्करण काफी समान है लेकिन `File` ऑब्जेक्ट बनाने के लिए क्लोज़र का `io/file` फंक्शन उपयोग करता है। यह तरीका क्लोज़र कोडबेस में अधिक स्वाभाविक रूप से फिट बैठता है, क्लोज़र की IO ऑपरेशन के लिए लाइब्रेरी का लाभ उठाकर, सीधे Java क्लासेस के साथ इंटरफेस करने के बजाय।