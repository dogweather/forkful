---
title:                "मानक त्रुटि के लिए लिखना"
aliases:
- hi/java/writing-to-standard-error.md
date:                  2024-02-03T19:34:33.205741-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
मानक त्रुटि (stderr) में लिखना यानी कंसोल या टर्मिनल पर त्रुटि सन्देशों और नैदानिक जानकारी को आउटपुट करना शामिल है। प्रोग्रामर इसे मानक आउटपुट (stdout) से त्रुटि जानकारी को अलग करने के लिए करते हैं, जिससे डीबगिंग और लॉग विश्लेषण में सहायता मिलती है।

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
