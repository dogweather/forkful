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

## क्या और क्यों?

HTTP अनुरोध भेजना एक ऐसा प्रक्रिया है जिसमे विभिन्न समर्थन प्रोटोकॉल का इस्तेमाल किया जाता है, जो सर्वर से सम्पर्क करके डेटा को जोड़ता है। संकल्पक इसका उपयोग अपने ऐप्लिकेशन को समर्थित करने के लिए करते हैं।

## कैसे करें:

```Clojure
(require '[clojure.data.json :as json])
(require '[clj-http.client :as client])

(def response (client/get "https://jsonplaceholder.typicode.com/posts"))

(json/read-str (:body response))
```
इस उदाहरण में, हम एक HTTP अनुरोध भेजने के लिए clj-http लाइब्रेरी का उपयोग करते हैं और प्रतिक्रिया में वापस आने वाले डेटा को जोड़ते हैं। यहां हमने jsonplaceholder.typicode.com पर उपलब्ध डेटा को हासिल किया है।

## गहरी जाँच:

HTTP अनुरोध भेजने का एक इतिहासिक पारिवारिक है जो 1991 में डी मेनट की ओर से बनाया गया था। अनुरोध पूर्ण रूप से कस्टमाइज किया जा सकता है और कई विभिन्न तरीकों में प्रदान किया जा सकता है जैसे कि POST, GET, PUT आदि। इसके अलावा, अन्य समर्थन प्रोटोकॉल जैसे FTP, SMTP आदि भी हैं जो भेजने के लिए उपयोगी हो सकते हैं। इसका एक उदाहरण ऑनलाइन शॉपिंग वेबसाइट है जहां आप अपने विवरण भेजकर अपने उत्पादों को खरीद सकते हैं।

## इससे संबंधित भी देखें:

- [HTTP अनुरोध के लिए Clj-http लाइब्रेरी का ऑफिशियल दस्तावेज़ा](https://github.com/dakrone/clj-http)
- [Clojure में HTTP अनुरोध कैसे भेजें](https://purelyfunctional.tv/guide/how-to-send-an-http-request-in-clojure/)
- [इंटरनेट प्रोटोकॉल के बारे में और जानें](https://www.brainbell.com/tutorials/Internet/Introduction_To_Internet_Protocols.htm)