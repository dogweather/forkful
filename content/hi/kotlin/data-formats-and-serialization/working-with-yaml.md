---
title:                "YAML के साथ काम करना"
aliases:
- /hi/kotlin/working-with-yaml/
date:                  2024-02-03T19:26:57.430176-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML, जिसका पूरा नाम YAML Ain't Markup Language है, एक उच्च पठनीय डेटा सीरियलाइजेशन प्रारूप है जिसका उपयोग अक्सर कॉन्फ़िगरेशन फ़ाइलों, डेटा संग्रहण, और अंतर-प्रक्रिया संदेशन के लिए किया जाता है। प्रोग्रामर्स अक्सर कॉन्फ़िगरेशन और सेटिंग्स को एक संरचित फिर भी सरल तरीके से प्रबंधित करने के लिए YAML के साथ काम करते हैं, JSON या XML के मुकाबले उसकी स्पष्टता और सादगी का लाभ उठाते हुए, जब पढ़नीयता महत्वपूर्ण होती है।

## कैसे करें:
Kotlin में YAML पारंगति और सीरियलाइजेशन के लिए अंतर्निर्मित समर्थन नहीं है, लेकिन आप `snakeyaml` (सामान्य YAML पार्सिंग के लिए) और `kotlinx.serialization` (एक YAML प्रारूप एक्सटेंशन के साथ) जैसे लोकप्रिय तृतीय-पक्ष लाइब्रेरीज का उपयोग करके YAML फाइलों के साथ काम कर सकते हैं।

### `snakeyaml` का उपयोग करना
**डिपेंडेंसी:**
```kotlin
implementation 'org.yaml:snakeyaml:1.30'
```

**YAML पढ़ें:**
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileInputStream

fun readYaml(filePath: String) {
    val yaml = Yaml()
    val inputStream = FileInputStream(filePath)
    val data = yaml.load<Map<String, Any>>(inputStream)

    println(data)
}

// नमूना प्रयोग
fun main() {
    readYaml("config.yaml")
}
```
**नमूना `config.yaml`:**
```yaml
database:
  host: localhost
  port: 5432
```
**नमूना आउटपुट:**
```
{database={host=localhost, port=5432}}
```
### YAML के साथ `kotlinx.serialization` का उपयोग 
सबसे पहले, यह सुनिश्चित करें कि आपके पास समुचित YAML समर्थन वाली `kotlinx-serialization` लाइब्रेरी है (यदि उपलब्ध है, क्योंकि `kotlinx.serialization` प्रमुखतः JSON और अन्य प्रारूपों को सीधे लक्षित करती है)।

**डिपेंडेंसी:**
```kotlin
// JSON के लिए (दृष्टांत में, YAML समर्थन या वैकल्पिक लाइब्रेरीज के लिए जाँचें)
implementation 'org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.2'
```

**एक धारावाहिक डेटा क्लास निर्धारित करें:**
```kotlin
import kotlinx.serialization.Serializable

@Serializable
data class Config(
    val database: Database
)

@Serializable
data class Database(
    val host: String,
    val port: Int
)
```

दुर्भाग्यवश, लेखन के समय, `kotlinx.serialization` में सीधे YAML समर्थन सीमित या विकसित हो सकता है। आपको एक मध्यवर्ती प्रतिनिधित्व (जैसे कि `snakeyaml` के साथ YAML को JSON में परिवर्तित करना और फिर `kotlinx.serialization` के साथ JSON का पार्सिंग करना) का उपयोग करने की आवश्यकता पड़ सकती है या `kotlinx.serialization` के साथ संगत समुदाय संचालित YAML सीरियलाइजेशन प्रोजेक्ट्स की तलाश करनी पड़ सकती है।

JSON के लिए, कोड कुछ इस तरह दिखेगा:
```kotlin
import kotlinx.serialization.json.Json
import kotlinx.serialization.decodeFromString

fun main() {
    val jsonText = """
    {
        "database": {
            "host": "localhost",
            "port": 5432
        }
    }
    """.trimIndent()
    
    val config = Json.decodeFromString<Config>(jsonText)
    println(config)
}
```

क्योंकि Kotlin और इसका पारिस्थितिक तंत्र लगातार विकसित हो रहा है, YAML समर्थन और लाइब्रेरियों के लिए नवीनतम जानकारी के लिए आधिकारिक दस्तावेज़ीकरण और समुदाय संसाधनों पर नज़र रखें।
