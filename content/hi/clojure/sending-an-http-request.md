---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
HTTP अनुरोध भेजना, एक सर्वर से डेटा अनुरोध करने की प्रक्रिया है। प्रोग्रामर्स इसे वेब API के साथ काम करने और वेबसाइटों या वेब सेवाओं से डेटा एक्सेस करने के लिए करते हैं।

## कैसे करें (How to)
HTTP अनुरोध भेजने के लिए, आप `clj-http` लाइब्रेरी का उपयोग कर सकते हैं। स्थापित करने के लिए, आपको अपने `project.clj` फ़ाइल में निम्नलिखित लाइन जोड़नी होगी:

```Clojure
[clj-http "3.12.0"]
```

फिर, आप अनुरोध भेजने के लिए `clj-http.client` के `get` मेथड का उपयोग कर सकते हैं:

```Clojure
(ns my-app.core
  (:require [clj-http.client :as client]))

(defn fetch-data []
  (let [response (client/get "http://example.com/api/data")]
    (println response)))
```
जब आप इस फ़ंक्शन को चलाते हैं, यह HTTP GET अनुरोध भेजता है और प्रतिक्रिया को मुद्रित करता है।

## गहरी डाइव (Deep Dive)
HTTP अनुरोध भेजने की क्षमता, 1990 में HTTP प्रोटोकॉल के साथ आयी। इसने वेब पर डाटा का आदान-प्रदान करने के नये तरीके साझा किए। विकल्प समाधानों में cURL और HTTPie शामिल हैं, लेकिन Clojure में, `clj-http` काफ़ी लोकप्रिय है।

## और देखें (See Also)
* clj-http documentation: https://github.com/dakrone/clj-http
* HTTP Specification: https://tools.ietf.org/html/rfc2616
* Other HTTP Libraries in Clojure: https://www.clojure-toolbox.com/