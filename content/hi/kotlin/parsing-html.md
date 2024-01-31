---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:34:17.028152-07:00
simple_title:         "HTML पार्स करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
HTML पार्सिंग वेब डेटा को संरचित प्रारूप में बदलने की प्रक्रिया है। प्रोग्रामर इसे वेबसाइटों से डेटा निकालने, बॉट्स बनाने, और स्क्रेपिंग के लिए करते हैं।

## कैसे करें? (How to:)
Kotlin में HTML पार्सिंग के लिए, आप `Jsoup` लाइब्रेरी का उपयोग कर सकते हैं। यह एक सिंपल उदाहरण है:

```kotlin
// Jsoup लाइब्रेरी का उपयोग करके HTML पार्सिंग
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>नमस्ते Kotlin</title></head><body><p>पार्सिंग HTML मजेदार है!</p></body></html>"
    val doc = Jsoup.parse(html)
    
    val title = doc.select("title").first().text()
    val pText = doc.select("p").first().text()
    
    println("टाइटल: $title")
    println("पैराग्राफ: $pText")
}

// आउटपुट:
// टाइटल: नमस्ते Kotlin
// पैराग्राफ: पार्सिंग HTML मजेदार है!
```

## गहराई से जानकारी (Deep Dive)
HTML पार्सिंग का इतिहास वेब के शुरुआती दिनों से जुड़ा हुआ है, जब ब्राउजर को वेब पेजों को सही प्रारूप में प्रस्तुत करने के लिए HTML को समझना था। आज, डेटा एक्सट्रेक्शन, ऑटोमेशन और साइबर सुरक्षा जैसे क्षेत्रों में इसका बड़ा महत्व है। पार्सिंग के लिए अन्य विकल्प में `html.parser`, `lxml` और अन्य पायथन लाइब्रेरीज शामिल हैं, लेकिन Kotlin/जावा डेवेलपर्स के लिए `Jsoup` एक पसंदीदा है जो शक्तिशाली और सहज है। `Jsoup` स्थानीय HTML फाइल्स को पढ़ने, URL से डॉक्यूमेंट्स फेच करने, और डोम एलिमेंट्स को संपादित करने की सुविधा देता है।

## और जानकारी (See Also)
- Jsoup Quickstart गाइड: [Jsoup Documentation](https://jsoup.org/)
- Kotlin प्रोग्रामिंग भाषा: [Kotlin Lang](https://kotlinlang.org/)
- HTML पार्सिंग गाइड: [W3C Parsing](https://www.w3.org/TR/html5/syntax.html#parsing)
- `lxml` पायथन लाइब्रेरी: [LXML Documentation](https://lxml.de/)
- `html.parser` - पायथन स्टैण्डर्ड लाइब्रेरी: [Python Official Docs](https://docs.python.org/3/library/html.parser.html)
