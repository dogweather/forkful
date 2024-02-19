---
aliases:
- /hi/java/parsing-html/
date: 2024-01-20 15:33:03.842761-07:00
description: "\u0939\u092E HTML \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\
  \u094D\u092F\u094B\u0902 \u0914\u0930 \u0915\u0948\u0938\u0947 \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902? HTML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917\
  \ \u0915\u093F\u0938\u0940 \u0935\u0947\u092C \u092A\u0947\u091C \u0915\u0940 HTML\
  \ \u092B\u093E\u0907\u0932 \u0915\u094B \u092A\u0922\u093C\u0915\u0930 \u0909\u0938\
  \u0915\u0947 \u0921\u0947\u091F\u093E \u0915\u094B \u0938\u0902\u0930\u091A\u0928\
  \u093E\u0924\u094D\u092E\u0915 \u0930\u0942\u092A \u0938\u0947 \u092A\u094D\u0930\
  \u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u0947 \u0915\u093E \u0915\u093E\u092E\
  \ \u0939\u094B\u0924\u093E \u0939\u0948\u0964\u2026"
lastmod: 2024-02-18 23:09:03.110692
summary: "\u0939\u092E HTML \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u094D\
  \u092F\u094B\u0902 \u0914\u0930 \u0915\u0948\u0938\u0947 \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902? HTML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0915\
  \u093F\u0938\u0940 \u0935\u0947\u092C \u092A\u0947\u091C \u0915\u0940 HTML \u092B\
  \u093E\u0907\u0932 \u0915\u094B \u092A\u0922\u093C\u0915\u0930 \u0909\u0938\u0915\
  \u0947 \u0921\u0947\u091F\u093E \u0915\u094B \u0938\u0902\u0930\u091A\u0928\u093E\
  \u0924\u094D\u092E\u0915 \u0930\u0942\u092A \u0938\u0947 \u092A\u094D\u0930\u093E\
  \u092A\u094D\u0924 \u0915\u0930\u0928\u0947 \u0915\u093E \u0915\u093E\u092E \u0939\
  \u094B\u0924\u093E \u0939\u0948\u0964\u2026"
title: "HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
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
