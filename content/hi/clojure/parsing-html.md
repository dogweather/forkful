---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:30:54.402634-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTML पार्सिंग यानी हाइपरटेक्स्ट मार्कअप लैंग्वेज डेटा को प्रोसेस करके स्ट्रक्चर्ड डेटा में बदलना। प्रोग्रामर्स ऐसा वेबसाइट्स से डेटा निकालने, और उसे विश्लेषण या दूसरे प्रोजेक्ट्स में इस्तेमाल करने के लिए करते हैं।

## How to: (कैसे करें?)
Clojure में HTML पार्सिंग के लिए `enlive` लाइब्रेरी एक लोकप्रिय विकल्प है। नीचे देखिए `enlive` का उपयोग करके कैसे HTML को पार्स किया जाता है:

```Clojure
(require '[net.cgrand.enlive-html :as html])

(defn parse-html
  [html-content]
  (html/html-resource (java.io.ByteArrayInputStream. (.getBytes html-content))))

(let [parsed-html (parse-html "<html><body><h1>Hello, World!</h1></body></html>")]
  (html/select parsed-html [:h1]))
```

उदाहरण का आउटपुट ऐसा होगा:

```Clojure
({:tag :h1, :attrs nil, :content ["Hello, World!"]})
```

## Deep Dive (गहराई से जानकारी)
HTML पार्सिंग का इतिहास वेब स्क्रैपिंग के उदय के साथ शुरू होता है, जो 1990 के दशक में लोकप्रिय हुआ। `enlive` के अलावा, Clojure में `jsoup` और `hickory` जैसे लाइब्रेरियां भी हैं, जो HTML पार्सिंग का काम करती हैं। पर इनके इंप्लीमेंटेशन अलग-अलग होते हैं। `enlive` एक डेक्लैरेटिव ऐप्रोच लेता है, `jsoup` जावा पर आधारित है, और `hickory` डीएसएल (डोमेन स्पेसिफिक लैंग्वेज) का उपयोग करता है। चुनाव आपके प्रोजेक्ट की जरूरतों पर निर्भर करता है।

## See Also (देखें भी)
- [Jsoup - a Java HTML parser](https://jsoup.org/)
- [Hickory - Clojure HTML Parser](https://github.com/davidsantiago/hickory)