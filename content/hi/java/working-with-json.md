---
title:                "Json के साथ काम करना"
html_title:           "Java: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON (JavaScript Object Notation) एक प्रसिद्ध डेटा प्रारूप है जो डेटा को संरचित और आसानी से समझने वाला बनाता है। यह हमारे युग में डेटा विनिमय का एक सरल तरीका है और इसकी मांग लगातार बढ़ रही है। इसलिए, जावा प्रोग्रामर के रूप में, यह JSON काम करना एक उपयोगी कौशल है।

## कैसे करें

जब हम JSON डेटा को अनावश्यक कोडिंग के लिए पारस्परिक रूप से बनाते हैं तो हमें कुछ बेहतरीन लाइब्रेरी द्वारा दिया जाता है। एक उदाहरण के लिए, हम निम्नलिखित कोड द्वारा अपने डेटा को एक JSON बनाने का उपयोग कर सकते हैं:
```Java 
JSONObject obj = new JSONObject();
obj.put("name", "John");
obj.put("age", 30);
obj.put("city", "New York");
System.out.println(obj);
```
इसका उत्पादन निम्नलिखित से होगा:
```
{"name": "John", "age": 30, "city": "New York"}
```

## गहराई में जाएं

JSON डेटा प्रारूप को समझने के लिए, हमें JSON ऑब्जेक्ट और उनके अतिरिक्त कुंजी और मानों के बारे में अधिक जानने की जरूरत होती है। इनको जब हमारे कोड में आना होता है, हम उन्हें प्रकारवार डेटा स्ट्रक्चर में बदल सकते हैं और अपने एप्लिकेशन में उपयोग कर सकते हैं। इसके अलावा, JSON के अन्य लंबे टाइप के डेटा प्रारूप जैसे XML और CSV के साथ भिन्नताएं और समानताएं समझना भी महत्वपूर्ण है।

## देखें भी

- [How to Parse JSON in Java](https://www.baeldung.com/java-json)
- [Introduction to JSON in Java](https://www.edureka.co/blog/json-in-java)