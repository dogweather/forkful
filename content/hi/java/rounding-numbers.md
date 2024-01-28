---
title:                "संख्याओं को पूर्णांक बनाना"
date:                  2024-01-26T03:47:32.266648-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णांक बनाना"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/rounding-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
संख्याओं को गोल करने का मतलब उन्हें निर्दिष्ट सटीकता की डिग्री में संशोधित करना होता है। प्रोग्रामर इसे पठनीयता के लिए संख्याओं को सरल बनाने, निश्चित विनिर्देशों को पूरा करने या निश्चित सीमाओं के भीतर गणनाओं को फिट करने के लिए करते हैं, जैसे कि फ़्लोटिंग-पॉइंट अंकगणित में सटीकता की त्रुटियों से बचना।

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
