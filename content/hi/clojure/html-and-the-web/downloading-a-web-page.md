---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:44:00.847994-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करना मतलब वेब से डाटा हासिल करना। प्रोग्रामर्स ऐसा डाटा प्रोसेसिंग, वेब स्क्रेपिंग या ऑफलाइन एनालिसिस के लिए करते हैं।

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
