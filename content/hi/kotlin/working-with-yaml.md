---
title:                "Kotlin: yaml के साथ काम करना"
simple_title:         "yaml के साथ काम करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

बहुत सारे प्रोग्रामिंग भाषाओं में कहानियों,डेटा और कॉन्फ़िगरेशन को स्टोर करने के लिए सामान्य रूप से YAML अपनाया जाता है। यह सटीक है, सरल और स्वादिष्ट है जो शायद आपको इसको पंजीकृत अधिक सुलभ बनाता है। आप कुछ हद तक YAML का उपयोग ज़रूर कर सकते हैं।

## कैसे

```Kotlin
val yaml = """ 
      - name: Ria
        age: 25
        city: Delhi
      - name: Monu
        age: 20
        city: Mumbai
""".trimIndent()
```

अब हम YAML स्ट्रिंग को YAMLObjet पर लोड करेंगे।

```Kotlin
val yamlObj = Yaml.default.decodeFromString<List<Person>>(yaml)
```

यहां, हम इस स्ट्रिंग में ज़रूरी फ़ील्ड्स को खोजेंगे। उदाहरण के लिए, हमारे फ़ील्ड्स "नाम", "आयु" और "शहर" हैं। हम YAML स्ट्रिंग को इसके तहत एक ब्यक्ति लिस्ट में जोड़ेंगे।

अब हम मूलभूत तरीके से एक नया YAML स्ट्रिंग बनाएंगे।

```
val newYaml = Yaml.default.encodeToString(yamlObj)
```

और हम इसे कमांड लाइन में देख सकते हैं।

```
println(newYaml)
```

आउटपुट:

```
- name: Ria
  age: 25
  city: Delhi
- name: Monu
  age: 20
  city: Mumbai 
```

## गहराई में

YAML को समझना बहुत आसान हो सकता है, लेकिन कुछ उपयोगी फीचर्स मुश्किल हो सकते हैं। YAML की समझ को अधिक सुरक्षित बनाने के लिए, आप युक्तियां, प्रकार और शैलियों को ठीक से समझना चाहिए। हमारी हाल की गहराई में अपनी सुविधा और प्रकार में हमारे कोड को बदलने की आवश्यकता हो सकती है। इसलिए, YAML के साथ काम करने से पहले, हमेशा इसकी गहराई म