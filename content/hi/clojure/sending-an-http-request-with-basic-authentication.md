---
title:                "Clojure: बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों
एचटीटीपी अनुरोध भेजने में आमतौर पर बेसिक प्रमाणीकरण के साथ संलग्न होने के लिए कोई वैध कारण है।

## कैसे
क्लोजर प्रोग्रामिंग में एचटीटीपी अनुरोध भेजने का सबसे सरल तरीका बेसिक प्रमाणीकरण का उपयोग करना है। नीचे दिए गए कोड ब्लॉक में एक उदाहरण है।

```Clojure
(require '[clj-http.client :as client])

(def response
  (client/post "http://example.com/api" ; वेबसाइट का URL दर्ज करें
    :body {"username" "John" ; आपका उपयोगकर्ता नाम
           "password" "12345"} ; आपका पासवर्ड
    :basic-auth ["username" "password"])) ; आपका उपयोगकर्ता नाम और पासवर्ड का उपयोग करें
```

इस उदाहरण का उत्पाद निम्नलिखित हो सकता है:

```Clojure
{:status 200 ; अनुरोध सफल था
 :headers {"Content-Type" "application/json"}
 :body "{\"message\": \"Hello, John!\"}"} ; सर्वर द्वारा भेजे गए संदेश का उत्तर
```

## गहराई में जाएं
एचटीटीपी अनुरोध भेजने के बारे में अधिक जानकारी के लिए, आप इसके संरूप और सिस्टम गणना प्रकारों को अधिक गहराई से समझ सकते हैं। हालांकि, यह किसी भी अनुरोध को सुरक्षित बनाने के लिए बहुत ही आवश्यक है।

## इससे जुड़े लिंक्स
[अधिक जानकारी के लिए आप Clojure की आधिकारिक वेबसाइट पर जा सकते हैं।](https://clojure.org/)
[अन्य एचटीटीपी अनुरोध भेजने के बारे में पढ़ने के लिए आप यह लेख देख सकते हैं।](https://www.httpwatch.com/httpgallery/authentication/)