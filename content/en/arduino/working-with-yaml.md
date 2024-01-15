---
title:                "Working with yaml"
html_title:           "Arduino recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

Are you looking for a more organized and readable way to store and work with data in your Arduino projects? Look no further than YAML! This simple yet powerful language allows you to structure your data in a human-friendly format, making it easier to understand and work with.

## How To

The first step is to install the Arduino-ESP32 library, which includes a YAML parsing library called "yaml_ard".

```Arduino
#include "yaml_ard.h"
```

Next, we need to create a YAML file to store our data. For example, let's say we have a file called "data.yaml" with the following content:

```YAML
name: Arduino
version: 1.8.13
platforms:
  - AVR
  - ARM
  - ESP32
```

To access this data in our code, we can use the ```yaml_ard_parse()``` function, passing in the filename and a callback function to handle the data.

```Arduino
void setup() {
  Serial.begin(9600); // initialize serial communication
  yaml_ard_parse("data.yaml", handle_data); // parse YAML file and pass data to callback function
}

void handle_data(yaml_ard_entry_t* entry) {
  Serial.println(entry->name); // print "Arduino"
  Serial.println(entry->version); // print "1.8.13"
  Serial.println(entry->platforms[2]); // print "ESP32"
}
```

As you can see, the data is now easily accessible through the ```entry``` variable. The example above also shows how to access nested data, in this case, the third element in the "platforms" array. 

## Deep Dive

YAML is a flexible language and allows for different data types, including strings, integers, booleans, arrays, and objects. You can also add comments in your YAML file using the "#" symbol.

Additionally, the ```yaml_ard_parse()``` function returns an error code, which can be used to handle any parsing errors. You can also create your own custom callback functions to handle specific data types.

## See Also

- [YAML Official Website](https://yaml.org/)
- [Arduino-ESP32 Library](https://github.com/espressif/arduino-esp32)