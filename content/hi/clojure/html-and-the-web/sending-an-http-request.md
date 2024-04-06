---
date: 2024-01-20 17:59:56.504357-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Clojure\
  \ \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `clj-http` \u0932\u093E\u0907\u092C\
  \u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0905\u0915\u094D\u0938\u0930 \u0915\u093F\u092F\u093E \u091C\u093E\
  \u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0938\u093E\
  \u0927\u093E\u0930\u0923 GET \u0905\u0928\u0941\u0930\u094B\u0927 \u0939\u0948."
lastmod: '2024-04-05T21:53:53.674027-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Clojure \u092E\u0947\
  \u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F `clj-http` \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0905\u0915\u094D\u0938\u0930 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0938\u093E\u0927\u093E\
  \u0930\u0923 GET \u0905\u0928\u0941\u0930\u094B\u0927 \u0939\u0948."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें:)
Clojure में HTTP अनुरोध भेजने के लिए `clj-http` लाइब्रेरी का इस्तेमाल अक्सर किया जाता है। यहाँ एक साधारण GET अनुरोध है:

```Clojure
(require '[clj-http.client :as client])

(def response (client/get "http://httpbin.org/get"))

(println response)
```

इस कोड का नतीजा कुछ ऐसा होगा:

```
{:status 200, :headers {...}, :body "..."}
```

पोस्ट अनुरोध करने का उदाहरण:

```Clojure
(def response (client/post "http://httpbin.org/post" {:form-params {:key "value"}}))

(println response)
```

इसका नतीजा भी कुछ ऐसा ही होगा, लेकिन बॉडी में पोस्ट डेटा दिखाई देगा।

## Deep Dive (गहन जानकारी)
HTTP अनुरोध भेजना वेबसाइटों की जानकारी पढ़ने और इंटरनेट संचार की बुनियादी क्रिया है। `clj-http` लाइब्रेरी Java की Apache HttpComponents लाइब्रेरी पर आधारित है, जो Clojure के लिए यह कार्यक्षमता प्रदान करती है। विकल्प के रूप में, `http-kit` और `aleph` जैसी लाइब्रेरियाँ भी हैं, जो अलग-अलग विशेषताएं और परफॉरमेंस के मापदंड पेश करती हैं। विस्तृत प्रोग्रामिंग में, आपको अनुरोधों के प्रकार, हेडर्स, बॉडी और रिस्पॉन्स को समझना पड़ सकता है।

## See Also (इसे भी देखें)
- `clj-http` GitHub Repo: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- HttpComponents: [https://hc.apache.org/](https://hc.apache.org/)
- `http-kit`: [http://www.http-kit.org/](http://www.http-kit.org/)
- `aleph`: [https://github.com/ztellman/aleph](https://github.com/ztellman/aleph)
- Official Clojure site: [https://clojure.org/](https://clojure.org/)
