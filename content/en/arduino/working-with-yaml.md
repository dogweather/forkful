---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML ain't markup language. It's a human-friendly data serialization standard for all programming languages. Programmers use it for config files, data exchange between languages, and it's easy to understand compared to XML or JSON.

## How to:
Arduino doesn’t handle YAML out-the-box. To work with it, you use an external library. For example:

Install the "ArduinoJson" library via Library Manager. Use `DynamicJsonDocument` for parsing:

```Arduino
#include <ArduinoJson.h>

const char* yaml = 
  "- title: The Catcher in the Rye\n"
  "  author: J.D. Salinger\n"
  "- title: Nineteen Eighty-Four\n"
  "  author: George Orwell\n";

void setup() {
  Serial.begin(9600);
  DynamicJsonDocument doc(1024);
  deserializeJson(doc, yaml);
  for (JsonObject elem : doc.as<JsonArray>()) {
    Serial.println(elem["title"].as<String>());
    Serial.println(elem["author"].as<String>());
  }
}

void loop() {
  // not used in this example
}
```

Sample output:

```
The Catcher in the Rye
J.D. Salinger
Nineteen Eighty-Four
George Orwell
```

## Deep Dive
YAML emerged in the early 2000s, built for human readability. As a JSON superset, any JSON file is also a valid YAML. Common alternatives include JSON or XML, but YAML’s minimal syntax aims for better human management without extra flaunt. Parsing YAML on Arduino means converting YAML to JSON using external tools and then using the JSON in your sketches.

## See Also
- Official YAML website: https://yaml.org
- ArduinoJson GitHub repository: https://github.com/bblanchon/ArduinoJson
- YAML to JSON online converter: https://www.json2yaml.com/
