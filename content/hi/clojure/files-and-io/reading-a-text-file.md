---
date: 2024-01-20 17:54:13.115277-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\
  \u0902\u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:53.707024-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\u0902\u092A\
  \u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

## How to: (कैसे करें:)
```Clojure
;; एक टेक्स्ट फाइल पढ़ने के लिए बेसिक उदाहरण
(with-open [rdr (clojure.java.io/reader "example.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```
सैंपल आउटपुट:
```
पहली पंक्ति
दूसरी पंक्ति
तीसरी पंक्ति
```
इस कोड से "`example.txt`" नामक फाइल की हर लाइन को पढ़ा जाता है और प्रिंट किया जाता है।

## Deep Dive (गहराई से जानकारी)
तारीखिक परिप्रेक्ष्य में, टेक्स्ट फाइल पढ़ने की प्रक्रिया उतनी ही पुरानी है जितनी कंप्यूटर प्रोग्रामिंग। विकल्पों में `slurp` संक्षिप्त फंक्शन है जो पूरी फाइल की सामग्री को एक स्ट्रिंग की तरह पढ़ता है। लेकिन `with-open` और `line-seq` का उपयोग करना बेहतर है हम आम तौर पर बड़ी फाइलों के साथ काम करते हैं क्योंकि यह मेमोरी उपयोगिता में कुशल होता है। `with-open` सुनिश्चित करता है कि रीडर सही ढंग से बंद हो, जबकि `line-seq` लेजी सीक्वेंस के रूप में लाइनों को एक-एक करके प्रोसेस करता है।

## See Also (और भी जानकारी)
- Clojure डॉक्स पर `line-seq`: [https://clojuredocs.org/clojure.core/line-seq](https://clojuredocs.org/clojure.core/line-seq)
- `slurp` के बारे में और जानकारी: [https://clojuredocs.org/clojure.core/slurp](https://clojuredocs.org/clojure.core/slurp)
- जावा आई/ओ का उपयोग कैसे करें (Clojure में): [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
