---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:33:03.842761-07:00
simple_title:         "HTML पार्स करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? 
हम HTML को पार्स क्यों और कैसे करते हैं?

HTML पार्सिंग किसी वेब पेज की HTML फाइल को पढ़कर उसके डेटा को संरचनात्मक रूप से प्राप्त करने का काम होता है। प्रोग्रामर ऐसा इसलिए करते हैं ताकि वे वेब पेज की सामग्री का विश्लेषण, संपादन या अन्य प्रकार के डेटा के साथ इंटीग्रेट कर सकें। 

## How to:
```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HtmlParserExample {
    public static void main(String[] args) {
        String html = "<html><head><title>First Parse</title></head>"
                    + "<body><p>Parsed HTML into a doc.</p></body></html>";
        Document doc = Jsoup.parse(html);
        Elements paragraphs = doc.select("p");
        for (Element paragraph : paragraphs) {
            System.out.println(paragraph.text());
        }
    }
}
```
यहाँ सैंपल आउटपुट:
```
Parsed HTML into a doc.
```

## Deep Dive
पार्सिंग HTML: इतिहास और विकल्प 

HTML पार्स करने की प्रक्रिया 1990 के दशक से है, जब वेब विकास ने रफ़्तार पकड़ी थी। इतिहास में, विभिन्न लाइब्रेरीज और टूल्स विकसित किए गए हैं, जैसे कि HTMLParser, Jsoup, और HtmlUnit। 

Jsoup का इस्तेमाल करने के विकल्पों में HtmlUnit और Java's own HtmlUnit जैसे लाइब्रेरीज शामिल हैं। Jsoup अन्य लाइब्रेरीज की तुलना में संपूर्ण DOM के साथ काम करता है, साथ ही साथ वेब पेज से फॉर्म सबमिट करने, URL से डेटा खींचने, और कुकीज मैनेज करने जैसी विशेषताएं प्रदान करता है।

पार्सिंग की अच्छाई यह है कि यह डेवलपर्स को HTML में विशिष्ट टैग्स, एट्रिब्यूट, और टेक्स्ट कंटेंट खोजने में सक्षम बनाता है। Jsoup में CSS सेलेक्टर सपोर्ट भी होता है, जिससे डोम एलिमेंट्स को खोजने का काम और भी सरल हो जाता है। 

## See Also
- [Jsoup Quick Start Guide](https://jsoup.org/)
- [Java HTML Parsing with Jsoup](https://www.baeldung.com/java-with-jsoup)
- [Java Document Object Model (DOM) Parsing](https://www.javatpoint.com/document-object-model-in-java)

इन लिंक्स पर जाकर आप और जानकारी ले सकते हैं कि जावा में HTML पार्सिंग कैसे की जाती है और Jsoup लाइब्रेरी का उपयोग करने में और क्या-क्या खासियतें हैं।
