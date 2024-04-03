---
date: 2024-01-26 04:24:01.057892-07:00
description: "TOML \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 Tom's Obvious,\
  \ Minimal Language\u0964 \u092F\u0939 \u090F\u0915 \u0921\u093E\u091F\u093E \u0938\
  \u0940\u0930\u093F\u092F\u0932\u093E\u0907\u091C\u093C\u0947\u0936\u0928 \u092A\u094D\
  \u0930\u093E\u0930\u0942\u092A \u0939\u0948 \u091C\u093F\u0938\u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917 \u092B\
  \u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\
  \u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0915\u093E\u2026"
lastmod: '2024-03-13T22:44:52.154082-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 Tom's Obvious, Minimal\
  \ Language\u0964 \u092F\u0939 \u090F\u0915 \u0921\u093E\u091F\u093E \u0938\u0940\
  \u0930\u093F\u092F\u0932\u093E\u0907\u091C\u093C\u0947\u0936\u0928 \u092A\u094D\u0930\
  \u093E\u0930\u0942\u092A \u0939\u0948 \u091C\u093F\u0938\u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917 \u092B\u093C\
  \u093E\u0907\u0932\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\u092F\
  \u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 \u092A\
  \u0922\u093C\u0928\u0947, \u0932\u093F\u0916\u0928\u0947 \u092E\u0947\u0902 \u0906\
  \u0938\u093E\u0928 \u0939\u094B\u0924\u093E \u0939\u0948 \u0914\u0930 \u090F\u0915\
  \ \u0939\u0948\u0936 \u091F\u0947\u092C\u0932 \u0915\u0947 \u0938\u093E\u0925 \u0905\
  \u091A\u094D\u091B\u0940 \u0924\u0930\u0939 \u0938\u0947 \u092E\u0948\u092A \u0915\
  \u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964."
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 39
---

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
