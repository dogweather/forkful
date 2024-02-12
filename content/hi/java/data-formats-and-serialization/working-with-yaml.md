---
title:                "YAML के साथ काम करना"
aliases:
- /hi/java/working-with-yaml.md
date:                  2024-02-03T19:26:09.690729-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML, जिसे "YAML Ain't Markup Language" के लिए छोटा किया जाता है, एक मानव-पठनीय डेटा सीरियलाइजेशन मानक है जिसे प्रोग्रामर कॉन्फ़िगरेशन फाइलों, डेटा डंपिंग, और भाषाओं के बीच डेटा संचारण के लिए उपयोग करते हैं। इसकी पठनीयता और उपयोग में आसानी के कारण यह लोकप्रिय है, जिससे यह एप्लिकेशन और सेवाओं को कॉन्फ़िगर करने के लिए एक सामान्य विकल्प बन जाता है।

## कैसे करें:
Java में, आप YAML फाइलों के साथ तृतीय-पक्ष लाइब्रेरीज़ का उपयोग करके काम कर सकते हैं क्योंकि Java Standard Edition में YAML के लिए निर्मित समर्थन शामिल नहीं है। एक लोकप्रिय लाइब्रेरी SnakeYAML है, जो आसानी से YAML डेटा को पार्स करने और उत्पन्न करने की अनुमति देती है।

### SnakeYAML सेटअप करना
सबसे पहले, अपने प्रोजेक्ट में SnakeYAML को शामिल करें। यदि आप Maven का उपयोग कर रहे हैं, तो अपनी `pom.xml` में निम्नलिखित निर्भरता जोड़ें:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### YAML पढ़ना
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
मान लें `config.yml` ऐसा दिखता है:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
आउटपुट होगा:
```
{name=Example, version=1.0, features=[login, signup]}
```

### YAML लिखना
जावा ऑब्जेक्ट्स से एक YAML उत्पन्न करने के लिए, SnakeYAML द्वारा प्रदान की गई `dump` विधि का उपयोग करें:
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
यह निम्नलिखित YAML सामग्री उत्पन्न और प्रिंट करेगा:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
SnakeYAML का उपयोग करके, Java डेवलपर्स आसानी से अपने एप्लिकेशनों में YAML पार्सिंग और जनरेशन को एकीकृत कर सकते हैं, YAML की पठनीयता और सादगी का लाभ उठाते हुए कॉन्फ़िगरेशन और डेटा एक्सचेंज उद्देश्यों के लिए।
