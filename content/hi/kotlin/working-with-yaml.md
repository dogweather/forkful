---
title:                "Yaml के साथ काम करना"
html_title:           "Kotlin: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?

YAML एक आसान से समझने और लिखने के लिए आपके डेटा को अभिव्यक्त करने का एक तरीका है। यह एक संख्या या टेक्स्ट देंगे जोते हुए फ़ॉरमैटिंग संरचना है, किस प्रकार आप एक फ़ाइल या डेटाबेस में सुरक्षित रूप से अपने डेटा को संग्रहीत और संरक्षित कर सकते हैं। यह एक कठिन उपकरण नहीं है और अधिकांश विकासकर्ताओं को इसे शामिल करना चाहिए।

## कैसे करें:

कॉट्लिन में YAML की प्रैक्टिस बहुत आसान है। साधारण गणना यह है कि संरचित डेटा एक कठिन संरचना है। उदाहरण के लिए, आइसी हऔंगरी यू एम एल द्वारा एक अनूठी उपकरण है।

```kotlin
val employee = """
    name: John Smith
    age: 35
    position: Manager
    salary: 50000
""".trimIndent()
```

इस उदाहरण में, हमने एक वाक्य डेटा स्ट्रिंग का उपयोग किया है और इसे वर्णन की उदाहरण के रूप में प्रयोग किया है। मानदंड वाक्य नाम, आयु, पद और वेतन शामिल हैं।

## गहरी खोज:

YAML का पहला संस्करण 2001 में आया था और इसे PyYAML (Python बिंदिंग) द्वारा लिखा गया था। आज, यह अनुवाद का मूल लक्ष्य सबसे अधिक प्रयुक्त की गई है। योग्य विकल्प जैसे कहाँ, कहाँ कॉट्लिन, और आइसी हौंगरनम जैसे कशिश्ट उपकरण प्रदान कर सकें।

## भी देखो:

- [Kotlin documentation](https://kotlinlang.org/docs/reference/basic-types.html)
- [YAML official website](https://yaml.org/)
- [PyYAML documentation](https://pyyaml.org/wiki/PyYAMLDocumentation)