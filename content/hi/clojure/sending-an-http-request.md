---
title:                "एक http अनुरोध भेजना"
html_title:           "Clojure: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Kyu
Kya aapko kabhi website par jaate waqt keval text aur images ke sath interact karna hai? Ya phir aapko kisi third-party API se data retrieve karna hai? In sab situations me, aapka program HTTP request ka upyog karke apne server se data lene me madad karta hai.

## Kaise Kare
```Clojure
(ns my-project.core
  (:require [clj-http.client :as client]))

;; Basic GET Request
(def response (client/get "https://www.example.com"))

;; POST Request with Header and Body
(def response
  (client/post "https://www.example.com"
             {:headers {"Content-Type" "application/json"}
              :body (json/write-str {:username "john" :password "12345"})}))

;; Retrieving response status code and body
(def status (client/status response))
(def body (client/string response))

```

## Deep Dive
HTTP request bhejne ke liye, aapko `clj-http.client` library ka upyog karna hoga, jo ki standard Clojure library me available nahi hai. Is library ka upyog karke aap GET, POST, PUT, DELETE jaise alag-alag HTTP methods ka upyog kar sakte hai. Aap header, body, query parameters aur cookies ko customize bhi kar sakte hai. Iske alawa, aap response ke status code, headers aur body ko bhi retrieve kar sakte hai.

## Dekhiye Bhi
* [clj-http Documentation](https://github.com/dakrone/clj-http#documentation)
* [HTTP Request Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
* [Understanding HTTP Requests and Responses](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)