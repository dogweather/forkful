---
date: 2024-01-20 17:44:49.707952-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0909\u0938\u0915\
  \u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u0905\u092A\u0928\
  \u0947 \u0915\u0902\u092A\u094D\u092F\u0942\u091F\u0930 \u092A\u0930 \u0938\u0939\
  \u0947\u091C\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0921\u0947\u091F\u093E \u090F\u0915\
  \u0924\u094D\u0930 \u0915\u0930\u0928\u0947, \u0935\u0947\u092C \u0938\u094D\u0915\
  \u094D\u0930\u0948\u092A\u093F\u0902\u0917, \u092F\u093E \u0911\u092B\u0932\u093E\
  \u0907\u0928 \u090F\u0928\u093E\u0932\u093F\u0938\u093F\u0938 \u0915\u0947 \u0932\
  \u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: '2024-03-11T00:14:26.007491-06:00'
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0909\u0938\u0915\u0940\
  \ \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u0905\u092A\u0928\u0947\
  \ \u0915\u0902\u092A\u094D\u092F\u0942\u091F\u0930 \u092A\u0930 \u0938\u0939\u0947\
  \u091C\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930 \u0907\u0938\u0947 \u0921\u0947\u091F\u093E \u090F\u0915\u0924\
  \u094D\u0930 \u0915\u0930\u0928\u0947, \u0935\u0947\u092C \u0938\u094D\u0915\u094D\
  \u0930\u0948\u092A\u093F\u0902\u0917, \u092F\u093E \u0911\u092B\u0932\u093E\u0907\
  \u0928 \u090F\u0928\u093E\u0932\u093F\u0938\u093F\u0938 \u0915\u0947 \u0932\u093F\
  \u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
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
