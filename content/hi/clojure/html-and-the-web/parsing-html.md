---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:23.275738-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Clojure \u092E\u0947\
  \u0902 HTML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0915\u0940 \u0915\
  \u094B\u0908 \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u0915\u094D\u0937\u092E\
  \u0924\u093E \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\u0915\u093F\u0928\
  \ \u0906\u092A Java \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C\
  \u093C \u092F\u093E Clojure \u0930\u0948\u092A\u0930 \u091C\u0948\u0938\u0947 \u0915\
  \u093F `enlive` \u092F\u093E `hickory` \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u2026"
lastmod: '2024-03-13T22:44:51.659759-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u092E\u0947\u0902 HTML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\
  \u0917 \u0915\u0940 \u0915\u094B\u0908 \u0928\u093F\u0930\u094D\u092E\u093F\u0924\
  \ \u0915\u094D\u0937\u092E\u0924\u093E \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\
  \u0947\u0915\u093F\u0928 \u0906\u092A Java \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940\u091C\u093C \u092F\u093E Clojure \u0930\u0948\u092A\u0930 \u091C\u0948\
  \u0938\u0947 \u0915\u093F `enlive` \u092F\u093E `hickory` \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u092F\u0939\u093E\u0901 \u0926\u094B\u0928\u094B\u0902 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902."
title: "HTML \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923"
weight: 43
---

## कैसे करें:
Clojure में HTML पार्सिंग की कोई निर्मित क्षमता नहीं है, लेकिन आप Java लाइब्रेरीज़ या Clojure रैपर जैसे कि `enlive` या `hickory` का उपयोग कर सकते हैं। यहाँ दोनों का उपयोग कैसे करें:

### Enlive का उपयोग करके:
Enlive HTML पार्सिंग और वेब स्क्रैपिंग के लिए लोकप्रिय विकल्प है। पहले, इसे अपनी परियोजना निर्भरताओं में शामिल करें:

```clojure
[net.cgrand/enlive "1.1.6"]
```

फिर, आप HTML को इस तरह पार्स और नेविगेट कर सकते हैं:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

यह स्निपेट एक HTML पेज को लाता है और क्लास `some-class` के साथ सभी `<div>` तत्वों को चुनता है।

आउटपुट इस तरह दिखाई दे सकता है:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Here's some content."]})
```

### Hickory का उपयोग करके:
Hickory Clojure में काम करने के लिए आसान एक प्रारूप में HTML को पार्स करने का तरीका प्रदान करती है। अपनी परियोजना निर्भरताओं में Hickory जोड़ें:

```clojure
[hickory "0.7.1"]
```

यहाँ एक सरल उदाहरण है:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; HTML को Hickory प्रारूप में पार्स करें
(let [doc (hickory/parse "<html><body><div id='main'>Hello, world!</div></body></html>")]
  ;; id 'main' वाले डिव का चयन करें
  (select/select (select/id "main") doc))
```

यह कोड एक सरल HTML स्ट्रिंग को पार्स करता है और ID `main` वाले एक `div` को ढूँढने के लिए एक CSS सेलेक्टर का उपयोग करता है।

उदाहरण आउटपुट:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Hello, world!"]}]
```

`enlive` और `hickory` दोनों Clojure में HTML पार्सिंग के लिए शक्तिशाली समाधान प्रदान करते हैं, `enlive` अधिक टेम्पलेटिंग पर ध्यान केंद्रित करता है और `hickory` डेटा परिवर्तन पर जोर देता है।
