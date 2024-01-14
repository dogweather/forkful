---
title:                "Clojure: कंप्यूटर प्रोग्रामिंग पर एक टेक्स्ट फ़ाइल कैसे लिखें"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

वर्तमान में डिजिटल युग में, पाठ फ़ाइल लिखना आपके लिए काफी महत्वपूर्ण हो सकता है। इससे आप अपने कोड को संग्रहीत कर सकते हैं और भविष्य में इस्तेमाल के लिए सुविधाजनक हो सकते हैं।

## कैसे करे

```Clojure
(with-open [file (clojure.java.io/writer "test.txt")]
  (.write file "हैलो दुनिया"))
```

उपरोक्त कोड उस फ़ाइल में "हैलो दुनिया" लिख देगा। आप अपनी फ़ाइल का पाठ और फ़ॉर्मेटिंग परिवर्तित कर सकते हैं जैसे कि आप चाहते हैं।

## गहराई में जाइए

पाठ फ़ाइल लिखना सिर्फ़ आपको अपने कोड को संग्रहीत करने के लिए नहीं है, बल्कि इससे आप अपने कोड को सुधारने में भी मदद मिल सकती है। आप इससे अपने कोड के साथ एक्सपेरिमेंट कर सकते हैं और बाद में वापस प्रवण कर सकते हैं। कई बार पाठ फ़ाइल में अपने कोड के स्निपेट्स भी जोड़े जाते हैं जो आपकी प्रोजेक्ट के लिए बहुत उपयोगी हो सकते हैं।

## देखें भी

- [Clojure Cheatsheet](https://clojure.org/api/cheatsheet)
- [Beginner's Guide to Clojure](https://clojure.org/guides/learn/syntax)
- [ClojureDocs](https://clojuredocs.org/)