---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या है और क्यों?)
JSON (JavaScript Object Notation) एक lightweight data-interchange format है जो human-readable होने के साथ साथ machine-readable भी है। Arduino programming में इसका उपयोग data को आसानी से exchange और store करने के लिए किया जाता है।

## How to: (कैसे करें?)
Arduino में JSON के साथ काम करने के लिए `ArduinoJson` library का इस्तेमाल किया जाता है। इसे `Tools > Manage Libraries` में जाकर install करें और फिर नीचे दिए गए कोड की मदद से JSON objects को parse और serialize करने का तरीका सीखें:

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // JSON Object बनाना
  StaticJsonDocument<200> doc;
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;

  // JSON Object में value जोड़ना
  JsonObject position = doc.createNestedObject("position");
  position["latitude"] = 37.421998333333335;
  position["longitude"] = -122.08400000000002;

  // Serialize करके output करना
  serializeJson(doc, Serial);
}

void loop() {
  // do nothing
}
```
Sample Output:
```
{"sensor":"gps","time":1351824120,"position":{"latitude":37.421998333333335,"longitude":-122.08400000000002}}
```

## Deep Dive (गहराई में):
JSON पहली बार 2002 में Douglas Crockford द्वारा popularize किया गया था। XML से हटकर, यह data को compact और readable format में store करता है। Arduino पर `ArduinoJson` सबसे लोकप्रिय library है, लेकिन अन्य alternatives जैसे कि `cJSON` या `aJson` भी मौजूद हैं। Implementation की बात करें तो, ArduinoJson dynamic memory का उपयोग करती है और इसे resource-constrained devices पर उपयोग करने हेतु fine-tune किया गया है।

## See Also (और भी देखें):
- ArduinoJson Library Documentation: https://arduinojson.org/
- JSON Introduction: https://www.w3schools.com/js/js_json_intro.asp
- JSON and Arduino tutorial: https://lastminuteengineers.com/parsing-json-arduino-tutorial/
