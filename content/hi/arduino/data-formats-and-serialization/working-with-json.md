---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:37.526257-07:00
description: "JSON, \u092F\u093E JavaScript Object Notation, \u090F\u0915 \u0939\u0932\
  \u094D\u0915\u093E \u0921\u0947\u091F\u093E \u0907\u0902\u091F\u0930\u091A\u0947\
  \u0902\u091C \u092B\u0949\u0930\u094D\u092E\u0947\u091F \u0939\u0948, \u091C\u094B\
  \ \u0907\u0938\u0947 Arduino \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\
  \u094D\u0938 \u092E\u0947\u0902 \u0921\u0947\u091F\u093E \u0938\u0902\u0917\u094D\
  \u0930\u0939\u0923 \u092F\u093E \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\
  \u0930\u0947\u0936\u0928 \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\u0947 \u0932\
  \u093F\u090F \u0906\u0926\u0930\u094D\u0936\u2026"
lastmod: '2024-02-25T18:49:50.010727-07:00'
model: gpt-4-0125-preview
summary: "JSON, \u092F\u093E JavaScript Object Notation, \u090F\u0915 \u0939\u0932\
  \u094D\u0915\u093E \u0921\u0947\u091F\u093E \u0907\u0902\u091F\u0930\u091A\u0947\
  \u0902\u091C \u092B\u0949\u0930\u094D\u092E\u0947\u091F \u0939\u0948, \u091C\u094B\
  \ \u0907\u0938\u0947 Arduino \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\
  \u094D\u0938 \u092E\u0947\u0902 \u0921\u0947\u091F\u093E \u0938\u0902\u0917\u094D\
  \u0930\u0939\u0923 \u092F\u093E \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\
  \u0930\u0947\u0936\u0928 \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\u0947 \u0932\
  \u093F\u090F \u0906\u0926\u0930\u094D\u0936\u2026"
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON, या JavaScript Object Notation, एक हल्का डेटा इंटरचेंज फॉर्मेट है, जो इसे Arduino प्रोजेक्ट्स में डेटा संग्रहण या कॉन्फ़िगरेशन फाइलों के लिए आदर्श बनाता है। प्रोग्रामर इसे इसकी सादगी और विभिन्न प्रोग्रामिंग वातावरणों में पठनीयता के कारण उपयोग करते हैं, जिसमें Arduino शामिल है, जो वेब APIs या अन्य सिस्टमों के साथ सहज डेटा इंटरचेंज को सक्षम बनाता है।

## कैसे:

Arduino में JSON के साथ कार्य करने के लिए, `ArduinoJson` लाइब्रेरी इसके उपयोग की सहजता और दक्षता के कारण एक लोकप्रिय विकल्प है। यह JSON स्ट्रिंग्स को पार्स करने, उनमें संशोधन करने, और वस्तुओं को वापस JSON स्ट्रिंग्स में सिरियलाइज़ करने की अनुमति देता है। इसका उपयोग कैसे करें, निम्न है:

1. **ArduinoJson लाइब्रेरी को इंस्टॉल करें**: Arduino IDE में लाइब्रेरी मैनेजर का उपयोग करें और "ArduinoJson" को इंस्टॉल करें।

2. **एक JSON स्ट्रिंग को डिसेरियलाइज करें**: यहाँ बताया गया है कि कैसे एक JSON स्ट्रिंग को पार्स किया जाए और मूल्यों को निकाला जाए।

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // JSON दस्तावेज़ के अनुसार आकार समायोजित करें
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float latitude = doc["data"][0]; // 48.756080
  float longitude = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // खाली loop
}
```

नमूना आउटपुट:

```
gps
1351824120
48.756080
2.302038
```

3. **एक JSON स्ट्रिंग में सिरियलाइज करें**: यहाँ बताया गया है कि कैसे डेटा से एक JSON स्ट्रिंग बनाई जाए।

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // डेटा के अनुसार आकार समायोजित करें
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // खाली loop
}
```

नमूना आउटपुट (पढ़ने के लिए स्वरूपित):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

`ArduinoJson` लाइब्रेरी का प्रभावी उपयोग Arduino प्रोजेक्ट्स को एक मानव-पठनीय प्रारूप में जटिल डेटा संरचनाओं को संवाद करने की अनुमति देता है, वेब सेवाओं के साथ विकास और एकीकरण को सुविधाजनक बनाता है।
