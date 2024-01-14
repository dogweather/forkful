---
title:                "Arduino: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-json.md"
---

{{< edit_this_page >}}

# क्यों

Arduino पर JSON काम करना बहुत महत्वपूर्ण है क्योंकि यह डाटा इनकोडिंग और ट्रांसफर के लिए एक मानक बना देता है। यह आसानी से डेटा को संग्रहीत करने और प्रोसेस करने में मदद करता है।

# कैसे करें

जेसॉन डेटा को आर्डुइनो में काम करने के लिए, हमें आर्डुइनो लाइब्रेरी को इन्स्टॉल करने की जरूरत होती है। इसके बाद, हमें ऑब्जेक्ट को बनाने के लिए JSON बाइट को इस्तेमाल करना होता है। नीचे दिए गए उदाहरण में, हमें साधारण JSON अर्रेंजमेंट को आर्डुइनो स्केच में कैसे इनिशियलाइज़ करें दिखाया गया है।

```Arduino
#include<ArduinoJson.h>

void setup() {
  // Initialize JSON object
  StaticJsonDocument<200> doc;
  // Add key-value pair to the object
  doc["name"] = "John";
  doc["age"] = 30;
  // Print the JSON object
  serializeJson(doc, Serial);
}

void loop() {

}
```

जब हम सीरियल मॉनिटर पर अपने आर्डुइनो से जुड़े हुए होते हैं, हमें निम्नलिखित आउटपुट मिलेगा। आप अपने अंतःस्थापित डेटा को डेटा विकसित करने से पहले देख सकते हैं।

```
{"name":"John", "age":30}
```

# गहराई में जाएं

जेसॉन प्रोटोकॉल को अध्ययन करने के लिए, आप डेटा संगतता और सार्वजनिकता की पूर्णता के साथ प्रभावी रूप से काम करने की ज़रूरत जान सकते हैं। हमारे आर्डुइनो बोर्ड के संग जेसॉन ओवर सेरियल प्रोटोकॉल को प्रयास करने के लिए आप निम्नलिखित पंक्तियों को प्रयोग कर सकते हैं।

```Arduino
#include<ArduinoJson.h>
#include<WiFiClientsSecure.h>

WiFiClientsSecure client;
StaticJsonDocument<250> doc;

doc["