---
title:                "Arduino recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
Are you looking for a way to easily organize and store data in your Arduino projects? Look no further than YAML! With this lightweight and versatile markup language, you can easily create human-readable data structures without worrying about the complexities of traditional programming languages.

## How To
To utilize YAML in your Arduino projects, follow these simple steps:

1. Install the YAML library in your Arduino IDE by going to **Sketch > Include Libraries > Manage Libraries** and searching for "YAML."
2. Import the library into your project by adding `#include <YAML.h>` at the top of your code.
3. Create a YAML document by declaring a `YAMLDoc` object.
4. Within the `setup()` function, add your data to the YAML document using the `add` method. For example: `yaml.add("name", "John");` This will create a key-value pair where the key is "name" and the value is "John."
5. In the `loop()` function, use the `>>` operator to print the YAML document to the serial monitor. For example: `Serial << yaml;` This will output the entire YAML document to the serial monitor.
6. You can also use the `get` method to retrieve specific data from the YAML document. For example: `String name = yaml.get("name");`

Here is an example of the full code:

```
ArduinoYAML yaml;

void setup() {
  Serial.begin(9600);
  YAMLDoc yaml;
  yaml.add("name", "John");
  Serial << yaml;
}

void loop() {
  // ...other code
}
```

The output in the serial monitor would look like this:

```
name: John
```

## Deep Dive
YAML has many advanced features that can make data organization even easier. For example, you can create nested data structures by using a combination of `add` and `beginNode` methods. You can also add arrays to your YAML document by using the `addArray` method.

Additionally, YAML allows you to create references to existing data, making it easy to reuse the same data throughout your document. You can also include comments within your YAML document to provide additional context for your data.

## See Also
To learn more about working with YAML in Arduino, check out these resources:

- [Official ArduinoYAML library documentation](https://github.com/inyei/ArduinoYAML/wiki)
- [Getting started with YAML and Arduino](https://create.arduino.cc/projecthub/inyei/managing-data-with-yaml-ad5047)
- [Intro to YAML: Easy configuration and data storage](https://learn.adafruit.com/advanced-serial-console-on-mac-and-linux/yaml-configuration-files)