---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

---

## क्या और क्यों?

HTML पार्सिंग वेब पेज से डेटा खोजने की प्रक्रिया है। प्रोग्रामर्स इसे वेब कंटेंट के डेटा विश्लेषण और माइनिंग के लिए करते हैं।

## कैसे करें:

यहां Kotlin कोड ब्लॉक के द्वारा HTML पार्सिंग के उदाहरण और उनके आउटपुट दिए गए हैं:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>मेरा पेज</title></head>" +
            "<body><p>यह मेरा पेज है</p></body></html>"
    val doc = Jsoup.parse(html)
    println("Head: ${doc.head()}")
    println("Title: ${doc.title()}")
    println("Body: ${doc.body()}")
}
```

आउटपुट

```
Head: <head><title>मेरा पेज</title></head>
Title: मेरा पेज
Body: <body><p>यह मेरा पेज है</p></body>
```

## गहरा गोता

1. **ऐतिहासिक प्रसंग:** समय के साथ, विभिन्न HTML पार्सर की विकास यात्रा हुई है। जैसा कि साइटों का संरचना और कंटेंट बदले, पार्सर इन बदलावों को समर्थन करने के लिए बदले।
   
2. **विकल्प:** Jsoup के अलावा अन्य लाइब्रेरी भी मौजूद हैं जैसे कि HtmlUnit, Jericho HTML Parser आदि, आपके योग्यताओं और आवश्यकताओं के अनुसार।
   
3. **क्रियान्वयन विवरण:** HTML पार्सर कोड में एक वेब पेज से जानकारी प्राप्त करके, उसे कुछ विशिष्ट फ़ॉर्मेट में बदल देते हैं। Jsoup, उदाहरण के लिए, DOM (Document Object Model) पद्धति का अनुसरण करता है।

## यह भी देखें:

1. [Jsoup डॉक्युमेंटेशन](https://jsoup.org/cookbook/extracting-data/selector-syntax)
2. [Kotlin डॉक्युमेंटेशन](https://kotlinlang.org/docs/home.html)
3. [HTMLUnit](http://htmlunit.sourceforge.net/)
4. [Jericho HTML Parser](https://jericho.htmlparser.net/docs/index.html)

---