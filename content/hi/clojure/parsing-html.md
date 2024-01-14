---
title:                "Clojure: एचटीएमएल पार्सिंग"
simple_title:         "एचटीएमएल पार्सिंग"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

HTML पार्सिंग में लिप्त होने का कारण हो सकता हैं कि आप वेब साइटों से डेटा अथवा जानकारी जुटाना चाहते हों। HTML पार्सिंग आपको आसानी से विभिन्न वेब पेजों से जानकारी निकालने में मदद करता हैं।

## कैसे करें

```Clojure
(ns parse-html.core
  (:require [clojure.data.xml :as xml]))

(defn parse-html [url]
  (let [html (slurp url)
        parsed (xml/parse-html html)]
    parsed))

(defn get-links [parsed-html]
  (let [links (xml/attr parsed-html :a :href)]
    (filter #(not (nil? %)) links)))

(defn get-heading [parsed-html]
  (let [heading (xml/text parsed-html)]
    heading))

(defn get-elements [parsed-html]
  (let [elements (xml/contents parsed-html)]
    elements))

(def url "https://example.com")

;; output:
;; Parse the HTML of a web page
(get-heading (parse-html url)) ;; "Welcome to Example"
(get-elements (parse-html url))
;; ("<p>Hello there!</p>" "<a href="https://example.com/about">About</a>")
(get-links (parse-html url)) ;; ("https://example.com/about")
```

## गहराई में जाएं

जब आप HTML पार्सिंग करते हैं, आप कुछ मुख्य चरणों को ध्यान में रखना चाहेंगे। सबसे पहले, आपको पेज को डाउनलोड करना होगा। इसके बाद, आपको उस HTML को पार्स करना होगा और उसमें प्रत्येक तुलनात्मक उपभाग को फॉर्मेट करना होगा। आप इसके बाद फॉर्मेटेड उपभागों का उपयोग करके डेटा और जानकारी को निकाल सकते हैं।

## इसके अलावा

- [HTML पार्सिंग विडियो ट्यूटोरियल](https://www.youtube.com/watch?v=5NAKOTy8sdA)
- [Clojure रिक्शा: HTML पार्सिंग](https://risc.io/blog/2017/05/13/html-parsing-in-clojure)
- [Clojure डॉक्यूमेंटेशन: clojure.data.xml](https://clojure.github.io/data.xml/)