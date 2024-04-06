---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:33.205741-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u091C\u093E\u0935\
  \u093E \u092E\u0947\u0902 `System.err.print()` \u092F\u093E `System.err.println()`\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 stderr \u092E\
  \u0947\u0902 \u0932\u093F\u0916\u0928\u0947 \u0915\u093E \u090F\u0915 \u0938\u0940\
  \u0927\u093E \u0924\u0930\u0940\u0915\u093E \u092A\u094D\u0930\u0926\u093E\u0928\
  \ \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u0906\u092A \u0907\u0938\u0947 \u0915\
  \u0948\u0938\u0947 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0935\u0939\u2026"
lastmod: '2024-03-13T22:44:52.142016-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u093E\u0935\u093E \u092E\u0947\u0902 `System.err.print()` \u092F\u093E\
  \ `System.err.println()` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0915\u0947 stderr \u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u0947 \u0915\u093E\
  \ \u090F\u0915 \u0938\u0940\u0927\u093E \u0924\u0930\u0940\u0915\u093E \u092A\u094D\
  \u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u0906\u092A\
  \ \u0907\u0938\u0947 \u0915\u0948\u0938\u0947 \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902 \u0935\u0939 \u092F\u0939\u093E\u0901 \u0939\u0948."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे करें:


### जावा में मूल Stderr आउटपुट
जावा में `System.err.print()` या `System.err.println()` का उपयोग करके stderr में लिखने का एक सीधा तरीका प्रदान करता है। आप इसे कैसे करते हैं वह यहाँ है:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("त्रुटि: शून्य से भाग नहीं किया जा सकता।");
        }
    }
}
```

नमूना आउटपुट:

```
त्रुटि: शून्य से भाग नहीं किया जा सकता।
```

यह सीधे मानक त्रुटि धारा में त्रुटि सन्देश प्रिंट करेगा।

### उन्नत त्रुटि हैंडलिंग के लिए लॉगर का उपयोग
जिन अनुप्रयोगों को अधिक सोफिस्टिकेटेड त्रुटि हैंडलिंग और लॉगिंग की आवश्यकता होती है, वे SLF4J के साथ Logback या Log4J2 जैसे लॉगिंग लाइब्रेरी का उपयोग करना आम है। यह त्रुटि आउटपुट को प्रबंधित करने में अधिक लचीलापन प्रदान करता है, जिसमें फाइल रीडायरेक्शन, फिल्टरिंग, और फॉर्मेटिंग शामिल है।

#### Logback के साथ उदाहरण
पहले, अपनी `pom.xml` (मेवन) या `build.gradle` (ग्रेडल) फाइल में Logback के लिए निर्भरता जोड़ें। मेवन के लिए:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

फिर, आप गलतियों को लॉग करने के लिए निम्नलिखित कोड का उपयोग कर सकते हैं:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("त्रुटि: शून्य से भाग नहीं किया जा सकता।", e);
        }
    }
}
```

यह कॉन्सोल या फाइल में, Logback कॉन्फ़िगरेशन के आधार पर, त्रुटि सन्देश के साथ स्टैक ट्रेस आउटपुट करेगा।

Logback जैसे लॉगिंग फ्रेमवर्क का उपयोग करके त्रुटि हैंडलिंग पर अधिक नियंत्रण प्रदान करता है, जिससे बड़े अनुप्रयोगों और प्रणालियों को प्रबंधित करना आसान हो जाता है।
