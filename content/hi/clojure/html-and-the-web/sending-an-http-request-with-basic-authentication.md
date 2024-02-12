---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
aliases:
- /hi/clojure/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:13.274482-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/sending-an-http-request-with-basic-authentication.md"
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
