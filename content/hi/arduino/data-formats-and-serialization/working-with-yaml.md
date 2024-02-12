---
title:                "YAML के साथ काम करना"
aliases: - /hi/arduino/working-with-yaml.md
date:                  2024-02-03T19:25:50.367778-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML (YAML Ain't Markup Language) एक मनुष्य-पठनीय डेटा सीरियलाइजेशन मानक है जिसे कॉन्फ़िगरेशन फ़ाइलें, अंतर-कार्यक्रम संचार, और डेटा संग्रहण के लिए उपयोग किया जा सकता है। प्रोग्रामर अपने अप्लिकेशनों की कॉन्फ़िगरेशन प्रक्रिया को सरल बनाने के लिए Arduino परियोजनाओं में YAML की ओर रुख करते हैं, जिससे कोड में गहराई से जाने बिना पैरामीटर्स को संशोधित करना आसान हो जाता है, पठनीयता में वृद्धि होती है, और कॉन्फ़िगरेशन को साझा करना सरल होता है।

## कैसे:

Arduino पर सीधे YAML के साथ काम करना, स्मृति सीमाओं और मूल YAML प्रसंस्करण पुस्तकालयों की अनुपस्थिति की वजह से, उच्च-स्तरीय प्रोग्रामिंग वातावरणों पर जितना सरल है उतना सरल नहीं है। हालाँकि, उन परियोजनाओं के लिए जिन्हें YAML पार्सिंग या जनरेशन की आवश्यकता होती है, एक सामान्य दृष्टिकोण में एक साथी कंप्यूटर (जैसे कि Raspberry Pi) का उपयोग करना या Arduino के लिए अधिक अनुकूल प्रारूप (जैसे JSON) में YAML फ़ाइलों को परिवर्तित करना शामिल होता है, जिसे बाहरी स्क्रिप्ट का उपयोग करके किया जाता है। प्रस्तुति उद्देश्य के लिए, एक लोकप्रिय पुस्तकालय, ArduinoJson का उपयोग करते हुए पिछले दृष्टिकोण पर ध्यान केंद्रित करते हैं।

**चरण 1:** अपनी YAML कॉन्फ़िगरेशन को JSON में परिवर्तित करें। आप ऑनलाइन उपकरणों या कमांड-लाइन उपयोगिताओं जैसे `yq` का उपयोग कर सकते हैं।

YAML फाइल (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

JSON में परिवर्तित हो गया (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**चरण 2:** ArduinoJson पुस्तकालय का उपयोग करके अपने Arduino स्केच में JSON फाइल को पार्स करें। सबसे पहले, आपको Arduino IDE में लाइब्रेरी मैनेजर के माध्यम से ArduinoJson पुस्तकालय इंस्टॉल करने की आवश्यकता है।

**चरण 3:** अपने कोड में JSON को लोड और पार्स करें। Arduino की स्टोरेज सीमाओं के कारण, कल्पना करें कि JSON स्ट्रिंग एक वेरिएबल में संग्रहीत है या एक एसडी कार्ड से पढ़ा जाता है।

उदाहरण Arduino स्केच:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Password: ");
  Serial.println(password);
}

void loop() {
  // इस उदाहरण के लिए यहाँ कुछ नहीं है
}
```

स्केच को चलाने पर आउटपुट:
```
SSID: YourSSID
Password: YourPassword
```

यह दृष्टिकोण, JSON में परिवर्तन और ArduinoJson पुस्तकालय के उपयोग को शामिल करते हुए, Arduino परियोजनाओं के भीतर प्रबंधनीय YAML कॉन्फ़िगरेशन हैंडलिंग की अनुमति देता है, माइक्रोकंट्रोलर पर सीधे YAML पार्सिंग के मुद्दों को दूर करके।
