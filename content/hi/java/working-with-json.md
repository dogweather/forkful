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

आपने कभी सोचा है कि प्रोग्रामिंग के दौरान जेसन का उपयोग क्यों किया जाता है? अगर हाँ, तो यह लेख आपके लिए है! इस लेख में हम जेवा करने के बारे में बात करेंगे जो कि हाल ही में हाल ही में जावा का एक नया संस्करण है। हम आपको इसके मदद से करने का तरीका सिखाएंगे और यह आपको क्यों करना चाहिए।

## क्या और क्यों ?

जेसन कार्य करने का सबसे अच्छा तरीका है जेक्सन डेटा को स्टोर और ट्रान्सफर करना। प्रोग्रामर्स के लिए, जेसन बहुत सरल और सुविधाजनक है। इसके साथ, यह पॉपुलर प्रोग्रामिंग भाषाओं में से एक है, जो इसे और भी आसान बनाता है।

## कैसे करे :

कैसे करने के लिए, हमारे पास दो विकल्प हैं - जैसों पार हो रहा है:
 `Java ...` कोड ब्लॉक के भीतर कोडिंग उदाहरण और सैंपल आउटपुट दिए गए हैं.
अपनी बातचीत सीखने के लिए निम्न कोड ब्लॉक का उपयोग करें:

`Map<String, String> myMap = new HashMap<>();
myMap.put("key1", "value1");
myMap.put("key2", "value2");`

जेवा का उपयोग करने के बारे में जानने के लिए, आगे बढ़ते हुए, हम `JSONObject` का उपयोग करेंगे जो हमारे स्ट्रिंग को प्रोसेस करने के लिए बनाया गया है। नीचे दिए गए उदाहरण में, हम जस्टन रूप से संरचित कर सकते हैं और `print` लूप के उपयोग से उन्हें देख सकते हैं।

```
import org.json.JSONArray;
import org.json.JSONObject;

JSONArray jsonArray = new JSONArray();
jsonArray.put("Apple");
jsonArray.put("Orange");

JSONObject jsonObject = new JSONObject();
jsonObject.put("fruits", jsonArray);

System.out.println(jsonObject.toString());
```

उपरोक्त कोड का आउटपुट निम्न होगा:

`{"fruits":["Apple","Orange"]}`

## डीप डाइव:

जेसन से काम करने का इतिहास इसे जादुई तरीके से स्टोरिंग और डेटा को इंटरफेस करने के लिए बनाया गया है। अन्य विकल्पों में से कुछ पॉपुलर भाषाएं, XML और CSV है। इनमें `JAXB` और `JAX-WS` भी शामिल हैं जो डेटा को सीख्यू कंप्यूटेशन और वेब सर्वसेज होटे है। जेसन की तुलना में, ये भाषाएं कंटेनिंग सेमांटिक डेटा के लिए समझने इत्यादि को बचें।

## देखिए:

 - [जेसन विकि में अधिक जानें](https://en.wikipedia.org/wiki/JSON)
 - [जेवा में जेसन अधिक जाने](https://www.javatpoint.com/java-json-tutorial)
 - [JAXB के बारे में जानें](https://www.oracle.com/java/technologies/java-architecture-for-xml-binding.html)