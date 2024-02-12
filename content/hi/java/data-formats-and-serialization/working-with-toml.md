---
title:                "TOML के साथ काम करना"
aliases:
- /hi/java/working-with-toml/
date:                  2024-01-26T04:24:01.057892-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML का मतलब है Tom's Obvious, Minimal Language। यह एक डाटा सीरियलाइज़ेशन प्रारूप है जिसका उपयोग कॉन्फ़िग फ़ाइलों के लिए किया जाता है। प्रोग्रामर इसका इस्तेमाल इसलिए करते हैं क्योंकि यह पढ़ने, लिखने में आसान होता है और एक हैश टेबल के साथ अच्छी तरह से मैप किया जा सकता है।

## कैसे करें:
आपको एक TOML पार्सिंग लाइब्रेरी की आवश्यकता होगी। मैं `toml4j` का सुझाव देता हूं। इसे अपने प्रॉजेक्ट में इस तरह से जोड़ें:

```java
// इसे अपने build.gradle में जोड़ें
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

यहां बताया गया है कि आप एक TOML फ़ाइल कैसे पार्स करें:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("सर्वर IP: " + ip);
        System.out.println("सर्वर पोर्ट: " + port);
    }
}
```

नमूना आउटपुट:

```
सर्वर IP: 192.168.1.1
सर्वर पोर्ट: 80
```

## गहराई से समझें
GitHub के सह-संस्थापक टॉम प्रेस्टन-वेर्नर द्वारा विकसित, TOML का उद्देश्य XML की तुलना में सरल और YAML से अधिक विस्तृत होना था। इसका नवीनतम संस्करण 1.0.0, जो 2021 में जारी हुआ था, एक स्थिर सेट ऑफ़ फीचर प्रदान करता है।

JSON या YAML जैसे विकल्प भी लोकप्रिय हैं। JSON डेटा इंटरचेंज के लिए बढ़िया है। YAML जटिल कॉन्फ़िग्स के लिए अधिक मानव-पठनीय है। TOML की ताकत इसकी सरलता और रस्ट समुदाय में इसका उपयोग है।

जब जावा के साथ TOML का उपयोग कर रहे हों, तो ध्यान दें कि आप जिस पार्सर का चयन करते हैं वह महत्वपूर्ण है। `toml4j` के आगे, कुछ `jackson-dataformat-toml` का विकल्प चुनते हैं। प्रत्येक की बारीकियां होंगी, जैसे कि त्रुटि हैंडलिंग या पार्सिंग प्रदर्शन, इसलिए अपने प्रोजेक्ट की आवश्यकताओं के आधार पर चुनें।

## देखें भी
- TOML विनिर्देश: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
