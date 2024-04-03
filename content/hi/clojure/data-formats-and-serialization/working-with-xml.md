---
date: 2024-01-26 04:30:41.072909-07:00
description: "\u0915\u0948\u0938\u0947: Clojure `clojure.data.xml` \u092A\u0941\u0938\
  \u094D\u0924\u0915\u093E\u0932\u092F \u0915\u094B XML \u092A\u093E\u0930\u094D\u0938\
  \u093F\u0902\u0917 \u0914\u0930 \u0909\u092E\u091F\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964 \u0938\u092C\u0938\u0947 \u092A\u0939\u0932\u0947, \u0906\u0907\u090F\
  \ \u0915\u0941\u091B XML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0947\u0902\
  ."
lastmod: '2024-03-13T22:44:51.705126-06:00'
model: gpt-4-0125-preview
summary: "Clojure `clojure.data.xml` \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\
  \u092F \u0915\u094B XML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0914\u0930\
  \ \u0909\u092E\u091F\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092A\u094D\u0930\
  \u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u0938\u092C\u0938\
  \u0947 \u092A\u0939\u0932\u0947, \u0906\u0907\u090F \u0915\u0941\u091B XML \u092A\
  \u093E\u0930\u094D\u0938 \u0915\u0930\u0947\u0902."
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

## कैसे:
Clojure `clojure.data.xml` पुस्तकालय को XML पार्सिंग और उमटने के लिए प्रदान करता है। सबसे पहले, आइए कुछ XML पार्स करें:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; XML स्ट्रिंग पार्स करें
  (println parsed))
```
आउटपुट:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Clojure संरचनाओं से XML उमटने के लिए:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
आउटपुट:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## गहराई में जाना
XML का इतिहास गहरा है, जो 90 के दशक के अंत में वेब डेटा के लिए SGML का एक सरलीकृत उपसमूह के रूप में शुरू हुआ था। SOAP और XHTML जैसी तकनीकों के साथ इसका उपयोग बढ़ गया लेकिन JSON से इसे थोड़ी प्रतिस्पर्धा मिली, जिसे इसकी हल्कापन और सादगी के लिए पसंद किया जाता है।

Clojure का XML के प्रति दृष्टिकोण इसे क्रियात्मक और डेटा-केंद्रित रखता है, भाषा की आत्मा के लिए सच रहता है। `clojure.data.xml` केवल एक विकल्प है; आपके पास बुनियादी जरूरतों के लिए `clojure.xml` है, और जावा इंटरोप के लिए, आप JAXB या DOM4J जैसे भारी-भरकम संगीत के साथ खेल सकते हैं।

ध्यान में रखें, बहुत बड़े XML दस्तावेज़ों के साथ निपटते समय प्रदर्शन और स्मृति अधिभार भारी हो सकता है। स्ट्रीमिंग पार्सर जैसे कि StAX मदद कर सकते हैं, लेकिन उनके लिए आपको जावा-लैंड में जाना होगा।

## और भी देखें
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API फ़ॉर XML प्रोसेसिंग (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
