---
title:                "पार्सिंग एचटीएमएल।"
html_title:           "Clojure: पार्सिंग एचटीएमएल।"
simple_title:         "पार्सिंग एचटीएमएल।"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/parsing-html.md"
---

{{< edit_this_page >}}

# क्या और क्यों?

पार्सिंग HTML क्या है? 
यह उपकरण कंप्यूटर भाषा में लिखा गया HTML को पढ़ने और समझने के लिए उपयोग किया जाता है। 
पार्सिंग HTML को अपने कोड में इस्तेमाल करके, प्रोग्रामर अपने एप्लिकेशन के साथ अंतरफलन फॉर्मैट से संबंधित समस्याओं को हल कर सकते हैं।

# कैसे करें?

```
Clojure (html/parser "<html>...</html>")
```

उपरोक्त उदाहरण में, हमने ```html``` फंक्शन को पुकारकर HTML के एक टैग के साथ एक स्ट्रिंग पास किया है। 
इससे प्रोग्रामर को HTML के मध्य से परिदृश्य परिवर्तन करने के लिए सुविधा मिलती है। 

## 
गहराई खोज:

1. संदर्भ के साथ इतिहासिक परिस्थिति: HTML पार्सिंग का मूल्य क्या है? 
2. वैकल्पिक विकल्प: Clojure के अलावा अन्य क्या समर्थन करता है? 
3. अंग्रेज़ी अनुवाद: पार्सिंग HTML से बाहर क्या अहम है?

# देखें कि:

https://github.com/netblob/html-xml-utils-clj
https://clojars.org/html-parser