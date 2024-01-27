---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

YAML एक डाटा सीरियलाइज़ेशन फॉर्मेट है जो डेटा को मनुष्य द्वारा पढ़े जाने योग्य फॉर्म में लिखने की सुविधा देता है। प्रोग्रामर्स कॉन्फ़िगरेशन फाइलों, सेटिंग्स, और डेटा को स्टोर व मैनेज करने के लिए YAML का उपयोग करते हैं।

## How to (कैसे करें):

कोटलिन में YAML को हैंडल करने के लिए आपको एक लाइब्रेरी की आवश्यकता होती है, जैसे 'snakeyaml'. पहले आप `build.gradle` में डिपेंडेंसी जोड़ें:

```kotlin
dependencies {
    implementation("org.yaml:snakeyaml:1.29")
}
```

फिर YAML को पढ़ने और लिखने के लिए साधारण कोड:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream
import java.io.FileWriter

fun main() {
    // YAML फाइल से डेटा पढ़ें
    val yaml = Yaml()
    val inputStream = FileInputStream("config.yaml")
    val data = yaml.load<Map<String, Any>>(inputStream)
    println(data)

    // YAML फाइल में डेटा लिखें
    val output = mapOf("name" to "मुकेश", "age" to 30)
    val writer = FileWriter("output.yaml")
    yaml.dump(output, writer)
}
```

यदि `config.yaml` इस प्रकार है:

```yaml
name: मुकेश
age: 30
```

उपरोक्त कोड का आउटपुट होगा:

```plain
{name=मुकेश, age=30}
```

## Deep Dive (गहराई में):

YAML ("YAML Ain't Markup Language" का संक्षिप्त रूप) 2001 में आया था, और यह JSON का एक अधिक पढ़ने योग्य विकल्प है। इसके अलावा, TOML और XML भी कॉन्फ़िगरेशन के लिए विकल्प हैं, पर YAML का प्रयोग इसकी सिंप्लिसिटी और रीडेबिलिटी के कारण अधिक होता है। कोटलिन में `snakeyaml` लाइब्रेरी का कार्य काफी अच्छे से देखा जा सकता है और YAML के साथ डेटा को सीरियलाइज़ और डीसीरियलाइज़ करना आसान बनाता है।

## See Also (इसे भी देखें):

- YAML स्पेसिफिकेशन के लिए इस लिंक पर जाएं: [YAML Official Specification](https://yaml.org/spec/1.2.2/)
- `snakeyaml` लाइब्रेरी के डॉक्यूमेंटेशन के लिए इस लिंक पर जाएं: [SnakeYAML Documentation](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- Kotlin की औपचारिक वेबसाइट: [Kotlin](https://kotlinlang.org/)
