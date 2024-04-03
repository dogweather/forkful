---
date: 2024-02-03 19:03:10.620226-07:00
description: "JSON, or JavaScript Object Notation, is a lightweight data interchange\
  \ format, making it perfect for data storage or configuration files in Arduino\u2026"
lastmod: '2024-03-13T22:45:00.340100-06:00'
model: gpt-4-0125-preview
summary: JSON, or JavaScript Object Notation, is a lightweight data interchange format,
  making it perfect for data storage or configuration files in Arduino projects.
title: Working with JSON
weight: 38
---

## How to:
To work with JSON in Arduino, the `ArduinoJson` library is a popular choice due to its ease of use and efficiency. It allows parsing JSON strings, modifying them, and serializing objects back into JSON strings. Here's how to use it:

1. **Install the ArduinoJson library**: Use the Library Manager in the Arduino IDE and install "ArduinoJson".

2. **Deserialize a JSON string**: Here's how to parse a JSON string and extract values.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // Adjust size according to the JSON document
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
  // Empty loop
}
```

Sample output:

```
gps
1351824120
48.756080
2.302038
```

3. **Serialize to a JSON string**: Here’s how to create a JSON string from data.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // Adjust size according to data
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // Empty loop
}
```

Sample output (formatted for readability):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

Using the `ArduinoJson` library effectively allows Arduino projects to communicate complex data structures in a human-readable format, facilitating development and integration with web services.
