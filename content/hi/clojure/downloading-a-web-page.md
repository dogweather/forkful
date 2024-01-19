---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वेब पेज डाउनलोड करना मतलब एक हुक बनाकर वेब सर्वर से डाटा उठाना, ताकि उसे बाद में पढ़ा या प्रसंस्करण किया जा सके। कार्यक्रम लेखक इसे डेटा एनालिटिक्स, स्क्रेपिंग, और जासूसी (spidering) के लिए करते हैं।

## कैसे करें:

इसे Clojure में करने के लिए ```http-kit``` लाइब्रेरी इस्तेमाल होती है।

```clojure
(ns scraper.core
  (:require [org.httpkit.client :as http]))

(defn download-page [url]
  (-> @(http/get url {:as :string})
      :body))

(println (download-page "http://example.com"))
```

यहाँ पर `println` के फंक्शन में output हमें `http://example.com` से HTML  दिखाएगा।

## गहरा डाइव

Clojure भाषा जवा वर्चुअल मशीन पर जीवित होती है, और इसाइलन्ट, डाटा केंद्रित और फंक्शनल भाषा है। "Lisp का एक दियलेक्ट" होने के कारण, इसे अक्सर हाइटिलोवित भाषाओं में गिना जाता हैं।

डाटा डाउनलोड करने के विकल्पों की बात करें तो, ```clj-http``` और ```http-kit``` का इस्तेमाल किया जा सकता है। लेकिन, ```http-kit``` का प्रदर्शन प्रशंसनीय है और इसे आसानी से प्रबंधित किया जा सकता है।

Clojure में वेब पेज डाउनलोड करने की क्रिया में, एक HTTP GET अनुरोध URL को भेजा जाता है, और फिर सर्वर उस URL की HTML का प्रतिसाद भेजता है।

## अधिक जानकारी के लिए:

- आधिकारिक Clojure डॉक्यूमेंटेशन: https://clojure.org/
- http-kit लाइब्रेरी: https://www.http-kit.org/
- Clojure हेतु Web पेज scraping: https://nakkaya.com/2012/01/25/simple-web-page-scraper-in-clojure/