---
title:                "मानक त्रुटि के लिए लिखना"
aliases: - /hi/kotlin/writing-to-standard-error.md
date:                  2024-02-03T19:34:37.147000-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

मानक त्रुटि (stderr) पर लिखने का अर्थ है त्रुटि संदेशों और निदानों को एक अलग धारा में आउटपुट करना, जो मानक आउटपुट (stdout) से अलग है, जिससे बेहतर त्रुटि संचालन और लॉग पार्सिंग संभव है। प्रोग्रामर इसे डीबगिंग को सुविधाजनक बनाने और यह सुनिश्चित करने के लिए करते हैं कि त्रुटि संदेशों की पहचान आसानी से की जा सके और यदि आवश्यक हो तो उन्हें पुन: निर्देशित किया जा सके, स्वच्छ आउटपुट लॉग्स या उपयोगकर्ता संदेशों को बनाए रखते हुए।

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
