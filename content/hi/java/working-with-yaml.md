---
title:                "यामल के साथ काम करना"
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML ("यामल") एक डेटा सीरियलाइजेशन फार्मेट है जो ह्यूमन-रीडेबल है। जावा प्रोग्रामर्स विन्यास फाइलें, डाटा इंटरचेंज, और ऐप्लिकेशन सेटिंग्स हेतु YAML का उपयोग करते हैं।

## How to: (कैसे करें:)
YAML को Java में पढ़ने के लिए, आपको Jackson या SnakeYAML जैसे लाइब्ररी की जरूरत होगी। यहां एक उदाहरण है SnakeYAML का उपयोग करते हुए:

```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        InputStream inputStream = YamlExample.class
          .getClassLoader()
          .getResourceAsStream("config.yaml");
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data.get("key"));
    }
}
```

यह कोड `config.yaml` फाइल से डेटा पढ़ता है और उसमें से "key" वैल्यू प्रिंट करता है।

## Deep Dive (गहराई से जानकारी)
YAML, "YAML Ain't Markup Language" के लिए है, जो एक रिकर्सिव एक्रोनिम है। YAML की शुरुआत 2001 में हुई थी और यह JSON का एक सुपरसेट है। YAML और JSON में कुछ समानताएँ हैं, लेकिन YAML में कमेंट्स और मल्टी-लाइन स्ट्रिंग्स के लिए बेहतर सपोर्ट होता है। विकल्पों में XML और JSON शामिल हैं, लेकिन YAML का उपयोग सरलता और पढ़ने में आसानी के लिए प्राथमिकता दी जाती है। YAML फ़ाइलों को पार्स करने में कंप्लेक्सिटी होती है, जिसे Java प्रोग्रामिंग लाइब्रेरीज़ का उपयोग करके सरल किया जा सकता है।

## See Also (संबंधित लिंक्स)
- YAML विकिपीडिया पेज: [https://en.wikipedia.org/wiki/YAML](https://en.wikipedia.org/wiki/YAML)
- SnakeYAML गाइड: [https://bitbucket.org/asomov/snakeyaml/wiki/Documentation](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- Jackson YAML गाइड: [https://github.com/FasterXML/jackson-dataformats-text/tree/master/yaml](https://github.com/FasterXML/jackson-dataformats-text/tree/master/yaml)
