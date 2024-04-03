---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:37.526257-07:00
description: "\u0915\u0948\u0938\u0947: Arduino \u092E\u0947\u0902 JSON \u0915\u0947\
  \ \u0938\u093E\u0925 \u0915\u093E\u0930\u094D\u092F \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F, `ArduinoJson` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940 \u0907\u0938\u0915\u0947 \u0909\u092A\u092F\u094B\u0917 \u0915\u0940\
  \ \u0938\u0939\u091C\u0924\u093E \u0914\u0930 \u0926\u0915\u094D\u0937\u0924\u093E\
  \ \u0915\u0947 \u0915\u093E\u0930\u0923 \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\
  \u0930\u093F\u092F \u0935\u093F\u0915\u0932\u094D\u092A \u0939\u0948\u0964 \u092F\
  \u0939 JSON \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938\u2026"
lastmod: '2024-03-13T22:44:52.810805-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u0930\u094D\u092F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, `ArduinoJson`\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0907\u0938\u0915\u0947\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0940 \u0938\u0939\u091C\u0924\u093E \u0914\
  \u0930 \u0926\u0915\u094D\u0937\u0924\u093E \u0915\u0947 \u0915\u093E\u0930\u0923\
  \ \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0935\u093F\u0915\
  \u0932\u094D\u092A \u0939\u0948\u0964 \u092F\u0939 JSON \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u092A\u093E\u0930\u094D\u0938\
  \ \u0915\u0930\u0928\u0947, \u0909\u0928\u092E\u0947\u0902 \u0938\u0902\u0936\u094B\
  \u0927\u0928 \u0915\u0930\u0928\u0947, \u0914\u0930 \u0935\u0938\u094D\u0924\u0941\
  \u0913\u0902 \u0915\u094B \u0935\u093E\u092A\u0938 JSON \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917\u094D\u0938 \u092E\u0947\u0902 \u0938\u093F\u0930\u093F\
  \u092F\u0932\u093E\u0907\u091C\u093C \u0915\u0930\u0928\u0947 \u0915\u0940 \u0905\
  \u0928\u0941\u092E\u0924\u093F \u0926\u0947\u0924\u093E \u0939\u0948\u0964 \u0907\
  \u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0948\u0938\u0947 \u0915\
  \u0930\u0947\u0902, \u0928\u093F\u092E\u094D\u0928 \u0939\u0948."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

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
