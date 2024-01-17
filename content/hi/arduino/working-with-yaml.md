---
title:                "Yaml के साथ काम करना"
html_title:           "Arduino: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रइयम्ल के साथ काम करना होता है, जानने के लिए दो से तीन लाइनों में यह प्रकार है (१) यह कैसे काम करता है, और (२) प्रोग्रामर इसे क्यों करते हैं।

क्या याम्ल काम करता है?
याम्ल (YAML) एक मानक स्ट्रक्टक डेटा फॉर्मेट है। प्रोग्रामर यह इस्तेमाल करते हैं ताकि वे अपने प्रोग्रामों में फॉरमेटेड डेटा दर्ज कर सकें। इससे उन्हें अधिक सुविधा और पढ़ने में अधिक आसानी होती है।

## कैसे करें?
```Arduino
#include <yaml.h>

// याम्ल सीधा फाइल खोलना
File config = SD.open("config.yaml");

// याम्ल से डेटा पढ़ना
YAML::Node node = YAML::Load(config);

// डेटा में वैल्यूज अक्षर स्थान पर पहुँचना
int value = node["key"]["subkey"].as<int>();

// अब आप डेटा का इस्तेमाल कर सकते हैं
Serial.println(value);

// याम्ल से डेटा लिखना
node["key"]["subkey"] = "new value";
YAML::Emitter emitter;
emitter << node;
File newConfig = SD.open("newConfig.yaml", FILE_WRITE);
newConfig.print(emitter.c_str());
```

## गहराई से जानें
याम्ल २००१ में एक मानक के रूप में बनाया गया था। दूसरी विकल्प के रूप में एक्स्टेंशन के साथ इसे भी जाना जाता है।याम्ल का सीधा काम करने के लिए, आपको हार्डवेयर कोडिंग जैसे SD कार्ड और रीडर से बात करने की आवश्यकता होती है।

## सीभी देखें
[आर्दुइनो होमपेज](https://www.arduino.cc/), [याम्ल होमपेज](https://yaml.org/), [याम्ल कार्यक्षमता के लिए स्रोत कोड](https://github.com/jbeder/yaml-cpp)