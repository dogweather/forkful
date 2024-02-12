---
title:                "XML के साथ काम करना"
aliases:
- /hi/clojure/working-with-xml.md
date:                  2024-01-26T04:30:41.072909-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML एक मार्कअप भाषा है जो दस्तावेजों को एन्कोड करने के लिए एक ऐसे तरीके में सक्षम बनाती है जो मानव और मशीन दोनों के लिए पठनीय होता है। यह वेब सेवाओं, कॉन्फिगरेशन फाइलों, और डेटा इंटरचेंज में कीलक है क्योंकि यह डेटा को एक संरचित, पदानुक्रमित प्रारूप में ले जाता है।

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
