---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

YAML एक आसान और मानव-पठनीय डाटा सीरियलाइजेशन फ़ॉर्मेट है। प्रोग्रामर्स YAML का उपयोग कॉन्फ़िगरेशन फ़ाइलों, डाटा आदान-प्रदान और सेटिंग्स स्टोर करने के लिए करते हैं क्योंकि इसे पढ़ना और लिखना आसान होता है।

## How to: (कैसे करें:)

Arduino पर YAML को सीधे पढ़ा या लिखा नहीं जाता है, लेकिन हम डाटा को संरचित कर सकते हैं जो YAML जैसा दिखता है। यहाँ एक उदाहरण है:

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // संरचित डाटा जो YAML की तरह दिखता है:
  StaticJsonDocument<200> config;
  config["name"] = "Arduino";
  config["version"] = 1.0;
  config["features"] = array;
  config["features"].add("Easy to use");
  config["features"].add("Flexible");
  config["features"].add("Extensible");

  // JSON को स्ट्रिंग में परिवर्तित करें:
  String output;
  serializeJson(config, output);

  // स्ट्रिंग को सीरियल मॉनिटर पर प्रिंट करें:
  Serial.println(output);
}

void loop() {
  // कुछ नहीं करना है
}
```

यह कोड जब Arduino पर चलेगा, सीरियल मॉनिटर में निम्न आउटपुट दिखेगा:

```
{"name":"Arduino","version":1,"features":["Easy to use","Flexible","Extensible"]}
```

## Deep Dive (गहन अध्ययन)

YAML, "YAML Ain't Markup Language" के लिए खड़ा है, जो एक आकर्षक पुनरावृत्ति शीर्षक है। 2001 में बनाया गया, यह JSON और XML के विकल्पों के रूप में लोकप्रिय हुआ है क्योंकि इसकी सादगी और पढ़ने में आसानी के कारण। Arduino वातावरण में, YAML का सीधा समर्थन नहीं है, परन्तु जेसन या खुद के बनाए पार्सर से हम इसके जैसे डाटा सीरियलाइजेशन का उपयोग कर सकते हैं। मुख्य चुनौती डिवाइस पर सीमित मेमोरी होती है, इसलिए हलके JSON पार्सर्स जैसे कि ArduinoJSON उपयोगी हैं।

## See Also (और जानकारी के लिए)

- ArduinoJSON library का दस्तावेज़ीकरण: https://arduinojson.org/
- YAML के आधिकारिक दस्तावेज़: https://yaml.org/
- JSON और YAML के बीच तुलना: https://json2yaml.com/
- Arduino के सीरियल वातावरण के लिए गाइड: https://www.arduino.cc/reference/en/language/functions/communication/serial/
