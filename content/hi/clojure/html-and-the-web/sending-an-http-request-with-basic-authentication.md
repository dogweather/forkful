---
aliases:
- /hi/clojure/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:01:13.274482-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0914\u0930 \u092C\u0941\u0928\
  \u093F\u092F\u093E\u0926\u0940 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\
  \u0923 \u0938\u0930\u0932 \u0939\u094B\u0924\u093E \u0939\u0948: \u0938\u0930\u094D\
  \u0935\u0930 \u0915\u094B \u092F\u0942\u091C\u0930\u0928\u0947\u092E \u0914\u0930\
  \ \u092A\u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u0947 \u0938\u093E\u0925 \u090F\
  \u0915 \u0930\u093F\u0915\u094D\u0935\u0947\u0938\u094D\u091F \u092D\u0947\u091C\
  \u0928\u093E \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0935\u0947\u092C \u0938\u0947\u0935\u093E\u0913\u0902 \u092E\u0947\
  \u0902\u2026"
lastmod: 2024-02-18 23:09:02.724648
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0914\u0930 \u092C\u0941\u0928\
  \u093F\u092F\u093E\u0926\u0940 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\
  \u0923 \u0938\u0930\u0932 \u0939\u094B\u0924\u093E \u0939\u0948: \u0938\u0930\u094D\
  \u0935\u0930 \u0915\u094B \u092F\u0942\u091C\u0930\u0928\u0947\u092E \u0914\u0930\
  \ \u092A\u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u0947 \u0938\u093E\u0925 \u090F\
  \u0915 \u0930\u093F\u0915\u094D\u0935\u0947\u0938\u094D\u091F \u092D\u0947\u091C\
  \u0928\u093E \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0935\u0947\u092C \u0938\u0947\u0935\u093E\u0913\u0902 \u092E\u0947\
  \u0902\u2026"
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध और बुनियादी प्रमाणीकरण सरल होता है: सर्वर को यूजरनेम और पासवर्ड के साथ एक रिक्वेस्ट भेजना होता है। प्रोग्रामर इसका उपयोग वेब सेवाओं में सुरक्षित एक्सेस के लिए करते हैं।

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
