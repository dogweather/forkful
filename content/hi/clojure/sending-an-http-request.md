---
title:                "Clojure: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

HTTP अनुरोध भेजना आज के डिजिटल युग में प्रचलित एक क्रिटिकल क्रिया है। यह अनुरोध उपयोगकर्ताओं द्वारा सेवाओं को कुछ भी मांगने के लिए होता है, जैसे वेब पेज, डेटा एपीआई या अन्य सामग्री। इसके अलावा, यह डेटा या गतिविधियों को भी प्राप्त करने का सबसे सुगम तरीका है।

## कैसे करें

अपने क्टोब को सम्बोधित करते हुए HTTP अनुरोध के प्रस्तुतिकरण करना बहुत आसान है। नीचे कुछ सामान्य उदाहरण हैं जो समझने में मदद करेंगे।

```Clojure
;; Simple GET request
(:require [clj-http.client :as client])
(println (client/get "https://example.com"))

;; POST request with data
(println (client/post "https://example.com/api/user"
                      {:body {:name "John" :age 25}}))

;; Custom header and query parameters
(println (client/get "https://example.com"
                     {:headers {"Authorization" "Token abc123"}
                      :query-params {:page 1 :limit 10}}))
```

आउटपुट:

```Clojure
;; GET
{:status 200
 :headers {"content-type" "text/html"}
 :body "<html>...</html>"}

;; POST
{:status 201
 :headers {"content-type" "application/json"}
 :body "{\"id\": 123, \"name\": \"John\", \"age\": 25}"}

;; GET with custom header and query parameters
{:status 200
 :headers {"content-type" "application/json"}
 :body "{\"results\": [...], \"total\": 100}"}
```

## गहराई से जाने

HTTP अनुरोध भेजने के लिए क्लोजरमें कई विकल्प उपलब्ध हैं। ये उपायों में से कुछ निम्न हैं:

- `clj-http` नामक लाइब्रेरी का उपयोग करना, जो यह एक सरल अपश्रृंखला और विशिष्ट फंक्शन शामिल करता है।
- `clj-http-lite` का इस्तेमाल करना, जो एक छोटा परन्तु अधिक पथ प्रदान करता है। यह प्रमाण लाइब्रेरी है जो केवल सर्वर संवाद करता हैं और उपयोगकर्ताओं को सर्वर द्वारा प्रदत्त जवाब को प्रक्रिया करने की अनुम