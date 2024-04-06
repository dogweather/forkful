---
date: 2024-01-20 17:44:00.847994-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Clojure\
  \ \u092E\u0947\u0902 `clj-http` \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\
  \u093F\u092F \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0939\u0948\
  \ web pages \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F\u0964 \u0928\u0940\u091A\u0947 \u090F\u0915 \u092C\
  \u0947\u0938\u093F\u0915 example \u0939\u0948."
lastmod: '2024-04-05T21:53:53.677223-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Clojure \u092E\u0947\
  \u0902 `clj-http` \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0939\u0948 web pages\
  \ \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F\u0964 \u0928\u0940\u091A\u0947 \u090F\u0915 \u092C\u0947\u0938\
  \u093F\u0915 example \u0939\u0948."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to: (कैसे करें:)
Clojure में `clj-http` एक लोकप्रिय लाइब्रेरी है web pages डाउनलोड करने के लिए। नीचे एक बेसिक example है:

```Clojure
(require '[clj-http.client :as client])

(defn download-web-page [url]
  (let [response (client/get url)]
    (:body response)))

(def url "http://example.com")
(println (download-web-page url))
```

यह कोड क्या करेगा? `http://example.com` से HTML content उठाएगा और console में print कर देगा।

## Deep Dive (गहन जानकारी)
पुराने ज़माने में हम `curl` या `wget` command line टूल्स का इस्तेमाल करते थे। Clj-http नया है लेकिन सरल और शक्तिशाली। ये Java के HttpClient के ऊपर बनाया गया है, इसलिए Clojure परफॉरमेंस और निर्भरता के मुद्दे कम रहते हैं। विकल्प में `http-kit` या `aleph` भी हो सकते हैं जो async capabilities ऑफर करते हैं।

## See Also (अन्य सूत्र)
- clj-http GitHub page: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Clojure for the Brave and True एक मुफ्त गाइड है जो Clojure सिखाता है: [https://www.braveclojure.com/](https://www.braveclojure.com/)
- Web scraping के लिए और भी बहुत कुछ है: Enlive और Hiccup Clojure के दो और libraries हैं जो HTML डाटा को प्रोसेस करने में मदद कर सकते हैं।
