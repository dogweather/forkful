---
title:                "वेब पृष्ठ डाउनलोड करना"
html_title:           "Clojure: वेब पृष्ठ डाउनलोड करना"
simple_title:         "वेब पृष्ठ डाउनलोड करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?
वेब पेज को डाउनलोड करना मतलब इंटरनेट से दूसरे कंप्यूटर पर उसकी सामग्री डाउनलोड करना। इसे प्रोग्रामर्स इस्तेमाल करते हैं ताकि वे इंटरनेट से अपने कंप्यूटर पर डेटा को आसानी से प्राप्त कर सकें।

## कैसे करें?
```Clojure
(require '[clojure.java.io :as io])
(defn download-page [url]
  (slurp (io/reader (java.net.URL. url))))
```
ऊपर दिए गए कोड फ़ंक्शन को प्रविष्ट URL के लिए कॉल करके पेज को डाउनलोड कर सकते हैं।

उदाहरण प्रयोग:
```Clojure
(download-page "https://google.com")
;; => "<!doctype html><html itemscope=\"\"... "
```

## गहराई में जायें
इस तकनीक के पीछे एक साल पुराने का अवधारणा है, जब प्रोग्रामर्स को अपने स्वयं के डेटा पर जल्द से और आसानी से पहुँच पाने की ज़रूरत हुई। इसके अलावा, दूसरे विकल्पों में वेब पेज को डाउनलोड करने के लिए कहीं ज्यादा प्रयोग करने वाला होगा। दूसरे प्रोग्रामिंग भाषाएं जैसे Python में भी वेब पेज को डाउनलोड करने के लिए प्रयोग किया जाता है।

## इससे जुड़े अन्य स्रोत
- [Clojure डॉक्यूमेंटेशन](https://clojure.org/api/cheatsheet)
- [Python में वेब पेज डाउनलोड करना](https://www.pythonforbeginners.com/python-on-the-web/how-to-download-a-file-with-python)