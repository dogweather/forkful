---
date: 2024-01-20 17:44:49.707952-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.111442-06:00'
model: gpt-4-1106-preview
summary: .
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
