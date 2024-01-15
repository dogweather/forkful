---
title:                "यामल के साथ काम करना"
html_title:           "Arduino: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि आप अपने Arduino बोर्ड को और अधिक उपयोगी बना सकते हैं? YAML और Arduino का उपयोग करके, आप अपने बोर्ड पर विभिन्न सेटिंग्स को एक आसान तरीके से सेट कर सकते हैं। इससे आपको आवश्यकता होने पर बोर्ड की सेटअप करने के लिए समय बचाने में मदद मिलेगी।

## कैसे

अपने आर्दुइनो बोर्ड में YAML फाइल का सेटअप करने के लिए, सबसे पहले आपको एक शुरुआती स्केच फाइल बनाना होगा। फिर, आपको 'yaml.h' लाइब्रेरी को अपने स्केच में शामिल करना होगा। नीचे दिए गए कोड ब्लॉक आपको यह स्पष्ट रूप से दर्शाते हैं:

```arduino
#include <yaml.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  // यहां YAML कोड को शामिल करें
  YAML::Node config = YAML::Load(R"(
  name: 'Arduino Uno'
  version: 3
  mode: 'manual'
  )");

  // कॉमेंट कर दी गई लाइनों को हटा कर सीरियल पोर्ट पर सेटिंग्स को प्रिंट करें
  Serial.print("Board Name: ");
  Serial.println(config["name"].as<String>());
  Serial.print("Version: ");
  Serial.println(config["version"].as<int>());
  Serial.print("Mode: ");
  Serial.println(config["mode"].as<String>());
  delay(5000);
}
```

आपको ऊपर दिए गए कोड ब्लॉक में YAML कोड को अपनी आवश्यकतानुसार संशोधित करना होगा। इसके बाद, आप अपने स्केच को अपने बोर्ड पर अपलोड कर सकते हैं। जब आप सीरियल मॉनिटर पर देखेंगे, तो आपको अपनी निर्दिष्ट सेटिंग्स को प्रिंट होते हुए देखेंगे।

## डीप डाइव

YAML कोड को Arduino में लोड करने के लिए, आपको 'yaml.h' लाइब्रेरी को अपनी स्केच में शामिल करना होगा। इस