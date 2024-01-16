---
title:                "बेसिक प्रमाणीकरण सहित http अनुरोध भेजना"
html_title:           "Clojure: बेसिक प्रमाणीकरण सहित http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण सहित http अनुरोध भेजना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

आपको क्यों एक HTTP अनुरोध को बेसिक प्रमाणीकरण के साथ भेजने में संलग्न होना चाहिए, इसका छोटा सा कारण है कि बेसिक प्रमाणीकरण सबसे सरल विधि है अपनी वेब अनुप्रयोगों को सुरक्षित बनाने के लिए। इसके अलावा, जब आपका अनुरोध कोई अन्य प्रणाली द्वारा प्रोसेस किया जाता है और आपकी अनुमति आवश्यक है, तो बेसिक प्रमाणीकरण आपको अपनी प्रावेदिक उपयोगकर्ताओं को पहचानने में मदद कर सकता है।

## हाउ टू

कोडिंग उदाहरण और सैंपल आउटपुट के साथ "```Clojure ...```" कोड ब्लॉक्स के भीतर, हम आपको सबसे सरल तरीके से बताएंगे कि आप कैसे Clojure का उपयोग करके HTTP अनुरोध को बेसिक प्रमाणीकरण के साथ भेज सकते हैं। यहां हम एक उदाहरण कोड दिखाएंगे जो आपको स्पष्ट रूप में दिखाएगा कि आपकैसे अपने अनुप्रयोग में भेजे गए HTTP अनुरोध को कैसे अनुरोध करने के लिए बेसिक प्रमाणीकरण का उपयोग कर सकते हैं।

```Clojure
(require '[clj-http.client :as httpClient])

(defn send-request-with-auth
  [url username password]
  (httpClient/with-basic-auth username password
    (httpClient/get url)))
```

जब आप यह कोड अपने Clojure REPL में चलाएंगे, तो आपको सफलतापूर्वक सर्वर से प्रतिक्रिया मिलनी चाहिए। आप आपके तरफ साथ भेजे गए हेडर्स को भी जांच सकते हैं, जो आपको आपके अनुमति उपयोगकर्ता को प्रमाणित करेंगे।

## डीप डाइव

बेसिक प्रमाणीकरण साधारण रूप से "उपय