---
title:                "Working with JSON"
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) involves manipulating data structured in a lightweight, text-based format that's easy for humans to read and write, and easy for machines to parse and generate. Programmers use JSON in Arduino projects to communicate with web services, exchange data, and configure devices seamlessly.

## How to:

To work with JSON in Arduino, you'll need the ArduinoJson library. Install it via Library Manager: Sketch > Include Library > Manage Libraries... then search for "ArduinoJson" and install.

Here's a simple example to parse JSON:

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);

  DynamicJsonDocument doc(1024);
  deserializeJson(doc, json);

  const char* sensor = doc["sensor"];
  long time = doc["time"];
  double latitude = doc["data"][0];
  double longitude = doc["data"][1];
  
  Serial.print("Sensor: ");
  Serial.println(sensor);
  Serial.print("Time: ");
  Serial.println(time);
  Serial.print("Latitude: ");
  Serial.println(latitude, 6);
  Serial.print("Longitude: ");
  Serial.println(longitude, 6);
}

void loop() {
  // Not used in this example.
}
```

Sample output:

```
Sensor: gps
Time: 1351824120
Latitude: 48.756080
Longitude: 2.302038
```

Creating JSON:

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  DynamicJsonDocument doc(1024);

  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  doc["data"][0] = 48.756080;
  doc["data"][1] = 2.302038;

  serializeJson(doc, Serial);
}

void loop() {
  // Not used in this example.
}
```

Sample output:

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

## Deep Dive

The ArduinoJson library, by Benoit Blanchon, became the de facto standard for JSON manipulation in Arduino. JSON gained popularity for its simplicity over XML, which was widely used before. Alternatives like MsgPack exist but JSON remains favorite for its text readability and widespread use. Implementation-wise, ensure you allocate enough memory for the `DynamicJsonDocument` to avoid overflows and use `StaticJsonDocument` for static or known-sized JSON objects.

## See Also

- ArduinoJson Library Documentation: https://arduinojson.org/
- JSON Official Website: https://www.json.org/json-en.html
- Arduino Forum for Discussions: https://forum.arduino.cc/
- Guide to Choosing Between StaticJsonDocument and DynamicJsonDocument: https://arduinojson.org/documentation/memory-model/
