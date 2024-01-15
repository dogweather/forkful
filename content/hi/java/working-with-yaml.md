---
title:                "Yaml के साथ काम करना"
html_title:           "Java: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

क्यों YAML के साथ काम करना है? YAML (YAML Ain't Markup Language) एक आसान और स्वचालित सुरक्षा के साथ स्ट्रक्चर्ड भाषा है, जो डेटा संरचना और विन्यास जैसे कार्यों को सरल बनाती है। यह संरचना में उतना अच्छा है कि यह कई अन्य भाषाओं के बीच विन्यास को संसाधित कर सकता है, जो इसे आसान बनाता है।

## कैसे करें

```Java
// Maven की मदद से YAML डिपेंडेंसी जोड़ें
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.27</version>
</dependency>
```

```Java
// YAML फ़ाइल को पढ़ें
File file = new File("data.yaml");
// YAML ऑब्जेक्ट परस्तुत करें
Yaml yaml = new Yaml();
// YAML फाइल से डेटा को लोड करें
Map<String, String> data = yaml.load(new FileInputStream(file));
```

```Java
// स्ट्रिंग स्ट्रीम में यूएसओं जोड़ें
StringWriter writer = new StringWriter();
yaml.dump(data, writer);
String output = writer.toString();
```

आप ऊपर दिए गए उदाहरण को अपनी जरूरतों के अनुसार संशोधित कर सकते हैं। अधिक जानकारी के लिए, [SnakeYAML documention](https://bitbucket.org/asomov/snakeyaml/src/default/README.markdown) का उपयोग कर सकते हैं।

## गहराई में

YAML संरचनाओं को पढ़ने और लिखने के लिए SnakeYAML के इर्द गिर्द कुछ अच्छे फीचर्स हैं। इनमें कुंजी शब्दों को क्षेत्रों में संग्रहीत करने, ट्रेस पहुँच के बारे में सूचना जुटाने, संभावित निर्देशिकाओं के साथ संख्या निर्धारण करने जैसे फीचर्स शामिल हैं। YAML दस्तावेज़ीकृत करने विशेषताओं की विस्तृत सूची के लिए, [SnakeYAML दस्तावेज़ीकृत लेखन](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation) का देखें।

## देख