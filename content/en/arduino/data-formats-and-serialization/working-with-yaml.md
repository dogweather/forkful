---
title:                "Working with YAML"
date:                  2024-02-03T19:03:09.945156-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML (YAML Ain't Markup Language) is a human-readable data serialization standard that can be used for configuration files, inter-program communication, and data storage. Programmers turn to YAML for Arduino projects to streamline the configuration process of their applications, making it easier to modify parameters without diving deep into code, enhancing readability, and making configuration sharing simpler.

## How to:

Working with YAML directly on Arduino is not as straightforward as on higher-level programming environments due to memory constraints and the absence of native YAML processing libraries. However, for projects that require YAML parsing or generation, a typical approach involves using a companion computer (like a Raspberry Pi) or converting YAML files to a more Arduino-friendly format (like JSON) using external scripts. For demonstration purposes, let’s focus on the latter approach using a popular library: ArduinoJson.

**Step 1:** Convert your YAML configuration to JSON. You can use online tools or command-line utilities like `yq`.

YAML file (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

Converted to JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**Step 2:** Use the ArduinoJson library to parse the JSON file in your Arduino sketch. First, you need to install the ArduinoJson library via the Library Manager in the Arduino IDE.

**Step 3:** Load and parse the JSON in your code. Due to Arduino's storage limitations, imagine the JSON string is stored in a variable or read from an SD card.

Sample Arduino sketch:
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
  // Nothing here for this example
}
```

Output upon running the sketch:
```
SSID: YourSSID
Password: YourPassword
```

This approach, involving conversion to JSON and leveraging the ArduinoJson library, allows for manageable YAML configuration handling within Arduino projects, circumventing direct YAML parsing on the microcontroller.
