---
title:                "वेब पेज डाउनलोड करना"
html_title:           "Clojure: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

Web पेज डाउनलोड करना किसी एक खास कारण से अच्छा हो सकता है। शायद आपको एक वेबसाइट के डिजाइन या फीचर्स से प्रभावित होना हो, या फिर आप अध्ययन करने के लिए इस्तेमाल करना चाहते हो। बहुत सारे मौके हैं जब आपको किसी वेब पेज को अपने सिस्टम पर डाउनलोड करने की आवश्यकता हो सकती है।

## कैसे करें?

आप Clojure का इस्तेमाल करके आसानी से किसी भी वेब पेज को आपके सिस्टम पर डाउनलोड कर सकते हैं। नीचे दिए गए Clojure कोड ब्लॉक में हम पांच स्टेप्स को देखेंगे जो आपको वेब पेज डाउनलोड करने में मदद करेंगे। आप अपनी आवश्यकताओं और ध्यान रखते हुए कोड में बदलाव कर सकते हैं।

```Clojure
;; स्टेप 1: dependencies जोड़ें
(defproject my-project "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-http "3.10.2"]])

;; स्टेप 2: निवेशन (विस्तारक) जोड़ें
(require '[clj-http.client :as client])

;; स्टेप 3: वेब पेज के लिए URL निर्धारित करें
(def url "https://www.example.com")

;; स्टेप 4: वेब पेज को डाउनलोड करें
(client/get url)

;; स्टेप 5: फाइल में वेब पेज का कोड सहेजें
(spit "webpage.html" (client/get url))
```

आप देख सकते हैं कि हमने `clj-http` डेपेंडेंसी जोड़ी है, जो एपीआई को आसानी से उपयोग करने में मदद करती है। हमने `client/get` फ़ंक्शन का उपयोग करके URL पर एक GET रिक्वेस्ट भेजा और उसे फाइल में सहेजा। आप वेब पेज को अपनी मर्जी के अनुसार अपने सिस्टम पर डाउनलोड कर