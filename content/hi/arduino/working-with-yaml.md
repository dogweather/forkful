---
title:                "Arduino: yaml के साथ काम करना"
simple_title:         "yaml के साथ काम करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

आर्डुइनो कोडिंग के साथ काम करने का काम YAML सलाहकार बहुत फायदेमंद है।

## कैसे करें

आगे दिए गए कोड उदाहरण में हम देखेंगे कि यमल कैसे दूसरे फाइल के साथ संवाद करता है। इसके पश्चात कोड ब्लॉक के अंदर आपको संदर्भ दिया जायेगा, जो आसानी से समझने के लिए बनाया गया है।

```Arduino
#include <SPI.h>
#include <SD.h>
#include <YAMLCPP.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // wait for serial port to connect. needed for native USB port boards only
  }

  Serial.print("Initializing SD card...");

  if (!SD.begin()) {
    Serial.println("initialization failed!");
    return;
  }
  Serial.println("initialization done.");

  File file = SD.open("sample.yaml");
  YAML::Node node = YAML::Load(file);
  file.close();

  std::string name = node["player"]["name"].as<std::string>();
  int age = node["player"]["age"].as<int>();

  Serial.println("Player Name: " + name);
  Serial.println("Age: " + age);
}

void loop() {
  // do nothing
}

```

आप कोड के बारे में बहुत सारी सबक प्राप्त करेंगे - जैसे कि फाइल से डेटा को कैसे लोड करे और आप डेटा को किस प्रकार से एक चर में एक्सेस कर सकते हैं।

## गहराई में जाएं

यमल प्रतीक्षा का नाम सदियों से कॉन्फ़िगरेशन, डेटा अभिप्राय और साधारण टेक्स्ट फाइल के लिए डेटा संरचना के लिए प्रयुक्त किया जाता है। लेकिन यह कम समझे जाने वाले प्रेरणादायक और कम लोअर राइसेस सूत्र है। यह मानते हैं कि इसको सीखने आसान है जो शुरुआतियों के लिए आसान होता है।

## देखें भी

- [आधिकारिक आर्डुइनो साइट](https://www.arduino.cc/)
- [आर्डुइनो के बारे में अधिक जानकारी](https://hi.wikipedia.org/wiki/%E0%A4%86%E0%A4%B0%E0%A5%8D%E0%A4%A1%E0%A5%81%E0%A4%8F%E0%A4%A8%E0%A5%8B) 
- [आ