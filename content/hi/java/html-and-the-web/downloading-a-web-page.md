---
date: 2024-01-20 17:44:49.707952-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0935\u0947\
  \u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930\
  \u0928\u093E \u0907\u0924\u093F\u0939\u093E\u0938 \u092E\u0947\u0902 \u092A\u0939\
  \u0932\u0940 \u092C\u093E\u0930 \u0924\u092C \u0936\u0941\u0930\u0941 \u0939\u0941\
  \u0906 \u091C\u092C \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0928\u092F\u093E\
  \ \u0925\u093E\u0964 \u0924\u092C \u0938\u0947, HTML, CSS, \u0914\u0930 \u091C\u093E\
  \u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F \u0915\u0947\
  \ \u091C\u091F\u093F\u0932\u0924\u093E \u092C\u0922\u093C \u0917\u090F \u0939\u0948\
  \u0902, \u0914\u0930\u2026"
lastmod: '2024-04-05T22:51:06.796020-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0935\u0947\u092C\
  \ \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930\u0928\
  \u093E \u0907\u0924\u093F\u0939\u093E\u0938 \u092E\u0947\u0902 \u092A\u0939\u0932\
  \u0940 \u092C\u093E\u0930 \u0924\u092C \u0936\u0941\u0930\u0941 \u0939\u0941\u0906\
  \ \u091C\u092C \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0928\u092F\u093E \u0925\
  \u093E\u0964 \u0924\u092C \u0938\u0947, HTML, CSS, \u0914\u0930 \u091C\u093E\u0935\
  \u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F \u0915\u0947 \u091C\
  \u091F\u093F\u0932\u0924\u093E \u092C\u0922\u093C \u0917\u090F \u0939\u0948\u0902\
  , \u0914\u0930 \u0907\u0938\u0932\u093F\u090F \u0935\u0947\u092C \u0938\u094D\u0915\
  \u094D\u0930\u0948\u092A\u093F\u0902\u0917 \u0915\u0940 \u0915\u0920\u093F\u0928\
  \u093E\u0908 \u092D\u0940\u0964 \u090F\u0915 \u0935\u093F\u0915\u0932\u094D\u092A\
  \ \u0939\u0948 \u091C\u093E\u0935\u093E \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940 Jsoup \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\
  \u093E, \u091C\u094B HTML \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0915\u0947 \u0938\u0930\u0932 \u092C\u0928\u093E\u0924\u093E \u0939\u0948\u0964\
  \ \u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u092E\u0947\u0902, \u092E\u0932\u094D\
  \u091F\u0940\u0925\u094D\u0930\u0947\u0921\u093F\u0902\u0917 \u0914\u0930 \u090F\
  \u0938\u093F\u0902\u0915\u094D\u0930\u094B\u0928\u0938 \u0935\u093F\u0927\u093F\u092F\
  \u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0935\u0947\u092C \u092A\
  \u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921\u093F\u0902\u0917 \u0915\
  \u094B \u0924\u0947\u091C \u0914\u0930 \u0915\u093E\u0930\u094D\u092F\u0915\u0941\
  \u0936\u0932 \u092C\u0928\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to: (कैसे करें:)
```java
import java.io.*;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) throws IOException {
        String webPageUrl = "http://example.com";
        URL url = new URL(webPageUrl);
        BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));
        BufferedWriter writer = new BufferedWriter(new FileWriter("downloaded_page.html"));

        String line;
        while ((line = reader.readLine()) != null) {
            writer.write(line);
        }

        reader.close();
        writer.close();
        System.out.println("वेब पेज डाउनलोड हो गया है।");
    }
}
```
सैंपल आउटपुट:
```
वेब पेज डाउनलोड हो गया है।
```

## Deep Dive (गहराई से जानकारी)
वेब पेज डाउनलोड करना इतिहास में पहली बार तब शुरु हुआ जब इंटरनेट नया था। तब से, HTML, CSS, और जावास्क्रिप्ट के जटिलता बढ़ गए हैं, और इसलिए वेब स्क्रैपिंग की कठिनाई भी। एक विकल्प है जावा लाइब्रेरी Jsoup का उपयोग करना, जो HTML को पार्स करके सरल बनाता है। वर्तमान में, मल्टीथ्रेडिंग और एसिंक्रोनस विधियों का उपयोग वेब पेज डाउनलोडिंग को तेज और कार्यकुशल बना सकता है।

## See Also (और जानकारी के लिए)
- [Jsoup प्रलेखन](https://jsoup.org/)
- [जावा नेटवर्किंग प्रलेखन](https://docs.oracle.com/javase/tutorial/networking/urls/index.html)
- [जावा मल्टीथ्रेडिंग गाइड](https://docs.oracle.com/javase/tutorial/essential/concurrency/)
