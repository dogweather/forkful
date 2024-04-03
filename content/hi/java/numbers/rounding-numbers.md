---
date: 2024-01-26 03:47:32.266648-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u091C\u093E\u0935\
  \u093E \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0917\u094B\
  \u0932 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0908 \u0924\
  \u0930\u0940\u0915\u0947 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0902 `Math.round()`, `BigDecimal`,\
  \ \u0914\u0930 `DecimalFormat` \u0915\u0947 \u0938\u093E\u0925 \u090F\u0915 \u0924\
  \u094D\u0935\u0930\u093F\u0924 \u0921\u0947\u092E\u094B \u0939\u0948\u0964."
lastmod: '2024-03-13T22:44:52.104957-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u093E\u0935\u093E \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u094B \u0917\u094B\u0932 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F \u0915\u0908 \u0924\u0930\u0940\u0915\u0947 \u092A\u094D\u0930\u0926\u093E\
  \u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0902 `Math.round()`,\
  \ `BigDecimal`, \u0914\u0930 `DecimalFormat` \u0915\u0947 \u0938\u093E\u0925 \u090F\
  \u0915 \u0924\u094D\u0935\u0930\u093F\u0924 \u0921\u0947\u092E\u094B \u0939\u0948\
  \u0964."
title: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u092A\u0942\
  \u0930\u094D\u0923\u093E\u0902\u0915 \u092C\u0928\u093E\u0928\u093E"
weight: 13
---

## कैसे करें:
जावा संख्याओं को गोल करने के लिए कई तरीके प्रदान करता है। यहां `Math.round()`, `BigDecimal`, और `DecimalFormat` के साथ एक त्वरित डेमो है।

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Math.round() का उपयोग करते हुए
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // आउटपुट: 123

        // अधिक नियंत्रण के लिए BigDecimal का उपयोग
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // आउटपुट: 123.46

        // DecimalFormat का उपयोग करते हुए
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // आउटपुट: 123.46
    }
}
```

## गहराई में जानकारी
ऐतिहासिक रूप से, संख्याओं को गोल करना ऍनालॉग गणनाओं के लिए आवश्यक रहा है और डिजिटल कंप्यूटिंग में दक्षता और सटीकता के लिए जारी रहा है। गोल करने की त्रुटियां, जैसे कि फ़्लोटिंग-पॉइंट अंकगणित से, दिखाती हैं कि यह एक साधारण मामला नहीं है - वे जमा होकर, उदाहरण के लिए, एयरोस्पेस और वित्तीय अनुप्रयोगों में गणनाओं को गड़बड़ कर सकते हैं।

`Math.round()` के अलावा, आपके पास `BigDecimal` है, जो आपको पैमाने और राउंडिंग मोड पर अधिक नियंत्रण देता है, और जब आपको पाठ आउटपुट का प्रारूपण करते समय संख्याओं को गोल करने की आवश्यकता होती है तो `DecimalFormat` होता है। गोल करने के विकल्प में फ्लोरिंग, सीलिंग, और ट्रंकेटिंग शामिल हैं, जो प्रेसिजन को संभालने के विभिन्न तरीके हैं और आमतौर पर विभिन्न `Math` विधियों द्वारा संभाले जाते हैं।

आपके उपयोग के मामले के आधार पर, गोल करने की रणनीति भिन्न हो सकती है। उदाहरण के लिए, वित्तीय गणनाओं में जहां सटीकता महत्वपूर्ण है, वहाँ `BigDecimal` का उपयोग किया जाता है। वहीं, `Math.round()` सामान्य उद्देश्य के संचालनों के लिए एक त्वरित तरीका है जहाँ आप राउंडिंग मोड के बारे में कम सजग होते हैं।

## देखें भी
- [ओरेकल का जावा माथ डॉक्यूमेंटेशन](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [फ्लोटिंग-पॉइंट अंकगणित के लिए IEEE मानक (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [जावा में DecimalFormat क्लास](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
