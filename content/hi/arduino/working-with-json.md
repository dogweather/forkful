---
title:                "Json के साथ काम करना"
html_title:           "Arduino: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-json.md"
---

{{< edit_this_page >}}

# क्यों

JSON (JavaScript Object Notation) एक सरल, आसानी से पढ़ने और लिखने के लिए होने का कारण, संरचनात्मक एक डेटा फॉर्मेट है। यह आर्डुइनो कोडिंग के लिए भी एक बेहतर विकल्प है, क्योंकि यह कंपैक्ट और उचित में से ज़्यादा परिवर्तनशील है। इसलिए, इस लेख में हम जानने वाले हैं कि Arduino में JSON कैसे लिखा और पढ़ा जाता है।

## कैसे करे

आर्डुइनो बोर्ड पर JSON एलागोरिथम को सभी विशेषताओं के साथ कॉम्पाइल करने के लिए हमें [ArduinoJson](https://github.com/bblanchon/ArduinoJson) पुस्तकालय को डाउनलोड करने की आवश्यकता होगी। जब आप इसे इनस्टाल कर लेते हैं, ```ArduinoJSON.h``` शामिल करना दर्शता है।

## गहराई में जाएँ

JSON को इलाकों में संग्रहीत करने के लिए, हमेंउसे ```JSONDocument``` डेटा प्रकार में बचाना चाहिए। आर्डुइनो में, हम हमेशा कंप्यूटर मेमरी में "JSONBuffer" का इंस्टेंस डालने की आवश्यकता होती है, यह हमें एक जल्दी विकल्प, जो डेटा कॉरपोरेट करता है, देता है, कोड देने के लिए आवश्यक है ताकि इसमें अतिरिक्त रूप से अवशोषण की आवश्यकता न हो।

आर्डुइनो बोर्ड पर जावास्क्रिप्ट के उपयोग का उदाहरण कुछ इस तरह का हो सकता है: 

  ```Arduino
//#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonBuffer<200> JSONBuffer; //मैमोरी में "ंग्रेसिव" का उपयोग करके स्तरीय प्रवाह दे
  JsonObject& JsonObject = JSONBuffer.decodeObject("{\"name\":\"John\", \"age\":30}");

  String name = JsonObject["name"];
  int age = JsonObject["age"];

  Serial.println(name);
  Serial.println(age);
}

void loop()