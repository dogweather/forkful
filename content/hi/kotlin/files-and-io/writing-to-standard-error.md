---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:37.147000-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Kotlin \u092E\u0947\
  \u0902, stderr \u092A\u0930 \u0932\u093F\u0916\u0928\u093E `System.err.println()`\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0915\u093F\
  \u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\
  \ \u0935\u093F\u0927\u093F `System.out.println()` \u0915\u0947 \u0938\u092E\u093E\
  \u0928 \u0939\u0948 \u0932\u0947\u0915\u093F\u0928 \u0906\u0909\u091F\u092A\u0941\
  \u091F \u0915\u094B\u2026"
lastmod: '2024-03-13T22:44:52.287260-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u092E\u0947\u0902, stderr \u092A\u0930 \u0932\u093F\u0916\u0928\u093E\
  \ `System.err.println()` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0915\u0947 \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0939 \u0935\u093F\u0927\u093F `System.out.println()` \u0915\
  \u0947 \u0938\u092E\u093E\u0928 \u0939\u0948 \u0932\u0947\u0915\u093F\u0928 \u0906\
  \u0909\u091F\u092A\u0941\u091F \u0915\u094B \u092E\u093E\u0928\u0915 \u0906\u0909\
  \u091F\u092A\u0941\u091F \u0927\u093E\u0930\u093E \u0915\u0947 \u092C\u091C\u093E\
  \u092F \u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0927\u093E\
  \u0930\u093E \u0915\u0940 \u0926\u093F\u0936\u093E \u092E\u0947\u0902 \u0928\u093F\
  \u0930\u094D\u0926\u0947\u0936\u093F\u0924 \u0915\u0930\u0924\u093E \u0939\u0948\
  \u0964."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे करें:
Kotlin में, stderr पर लिखना `System.err.println()` का उपयोग करके किया जा सकता है। यह विधि `System.out.println()` के समान है लेकिन आउटपुट को मानक आउटपुट धारा के बजाय मानक त्रुटि धारा की दिशा में निर्देशित करता है।

```kotlin
fun main() {
    System.err.println("यह एक त्रुटि संदेश है!")
}
```

नमूना आउटपुट:
```
यह एक त्रुटि संदेश है!
```

अधिक संरचित या जटिल अनुप्रयोगों के लिए, विशेष रूप से लॉगबैक या SLF4J जैसे लॉगिंग फ्रेमवर्क शामिल करने वालों के लिए, आप निश्चित लॉग स्तरों (जैसे, ERROR) के लिए stderr पर लिखने के लिए लॉगर्स को कॉन्फ़िगर कर सकते हैं।

SLF4J का उपयोग करते हुए Logback के साथ:

1. पहले, अपने `build.gradle` में SLF4J API और Logback कार्यान्वयन जोड़ें:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. अगला, Logback को कॉन्फ़िगर करें (इसे `src/main/resources/logback.xml` में) ताकि त्रुटि-स्तर के संदेशों को stderr पर निर्देशित किया जा सके:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. फिर, अपने Kotlin कोड में त्रुटि संदेश लॉग करने के लिए SLF4J का उपयोग करें:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("यह एक त्रुटि लॉग संदेश है!")
}
```

नमूना आउटपुट (स्टडेर पर):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - यह एक त्रुटि लॉग संदेश है!
```
