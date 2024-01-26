---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:44:49.707952-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करना मतलब उसकी सामग्री को अपने कंप्यूटर पर सहेजना है। प्रोग्रामर इसे डेटा एकत्र करने, वेब स्क्रैपिंग, या ऑफलाइन एनालिसिस के लिए करते हैं।

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
