---
date: 2024-01-20 18:01:13.274482-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Clojure \u092E\u0947\
  \u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0914\u0930 \u092C\u0941\u0928\
  \u093F\u092F\u093E\u0926\u0940 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\
  \u0923 \u0915\u0947 \u0932\u093F\u090F `clj-http` \u0932\u093E\u0907\u092C\u094D\
  \u0930\u0947\u0930\u0940 \u0909\u092A\u092F\u094B\u0917\u0940 \u0939\u0948\u0964\
  \ \u0909\u0926\u093E\u0939\u0930\u0923 \u0915\u094B\u0921."
lastmod: '2024-03-13T22:44:51.662946-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0914\
  \u0930 \u092C\u0941\u0928\u093F\u092F\u093E\u0926\u0940 \u092A\u094D\u0930\u092E\
  \u093E\u0923\u0940\u0915\u0930\u0923 \u0915\u0947 \u0932\u093F\u090F `clj-http`\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0909\u092A\u092F\u094B\
  \u0917\u0940 \u0939\u0948\u0964\n\u0909\u0926\u093E\u0939\u0930\u0923 \u0915\u094B\
  \u0921."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें:
Clojure में HTTP अनुरोध और बुनियादी प्रमाणीकरण के लिए `clj-http` लाइब्रेरी उपयोगी है।
उदाहरण कोड:
```Clojure
(require '[clj-http.client :as client])

(defn fetch-secure-data []
  (client/get "http://example.com/secure-data"
              {:basic-auth ["username" "password"]}))

(fetch-secure-data)
```

सैंपल आउटपुट:
```Clojure
{:status 200, :headers {...}, :body "..."}
```

## गहराई से जानकारी
HTTP अनुरोध में बुनियादी प्रमाणीकरण का विचार 1990 के दशक से विकसित हुआ। इसके विकल्पों में OAuth और API कुंजियां हैं। `clj-http` लाइब्रेरी `Authorization` हैडर का प्रयोग करती है, जिसमें Base64 एन्कोडेड यूजरनेम:पासवर्ड होता है, यह सुरक्षा के लिहाज से कमजोर है अगर सुरक्षित कनेक्शन (HTTPS) का उपयोग न किया जाए।

## संबंधित स्रोत
- `clj-http` लाइब्रेरी: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Clojure डॉक्स: [https://clojure.org/](https://clojure.org/)
- HTTP बुनियादी प्रमाणीकरण के बारे में MDN का आलेख: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
