---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T17:59:56.504357-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/sending-an-http-request.md"
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
