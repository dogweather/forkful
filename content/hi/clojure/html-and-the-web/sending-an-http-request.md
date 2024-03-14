---
date: 2024-01-20 17:59:56.504357-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u092E\u0924\u0932\u092C \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\
  \u0947 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0901\u0917\u0928\
  \u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930 \u0910\u0938\u093E \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\
  \u094D\u0924 \u0915\u0930\u0928\u0947, API \u0938\u0947 \u092C\u093E\u0924\u091A\
  \u0940\u0924 \u0915\u0930\u0928\u0947, \u0914\u0930 \u0926\u0942\u0930\u0938\u094D\
  \u0925 \u0938\u0947\u0935\u093E\u0913\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0938\
  \u0902\u0935\u093E\u0926 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \u2026"
lastmod: '2024-03-13T22:44:51.658143-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u092E\u0924\u0932\u092C \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\
  \u0947 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0901\u0917\u0928\
  \u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930 \u0910\u0938\u093E \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\
  \u094D\u0924 \u0915\u0930\u0928\u0947, API \u0938\u0947 \u092C\u093E\u0924\u091A\
  \u0940\u0924 \u0915\u0930\u0928\u0947, \u0914\u0930 \u0926\u0942\u0930\u0938\u094D\
  \u0925 \u0938\u0947\u0935\u093E\u0913\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0938\
  \u0902\u0935\u093E\u0926 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \u2026"
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना मतलब वेब सर्वर से जानकारी माँगना है। प्रोग्रामर ऐसा डेटा प्राप्त करने, API से बातचीत करने, और दूरस्थ सेवाओं के साथ संवाद करने के लिए करते हैं।

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
