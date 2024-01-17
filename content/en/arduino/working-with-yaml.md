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

## What & Why?

Working with YAML is a way for programmers to organize and store data in a human-readable format. It is often used for configuring settings or storing data in applications. Programmers use YAML because it allows them to easily edit and update data without the need for complex coding.

## How to:

To use YAML in your Arduino program, you will first need to download the Arduino YAML library. Once you have the library, you can include it in your code by adding the following line at the top:

```Arduino
#include <YAML.h>
```

Next, you can create a YAML document by declaring a YAML object:

```Arduino
YAML::Node document;
```

You can then add data to your YAML document using the appropriate data type:

```Arduino
document["name"] = "John Doe";
document["age"] = 30;
document["city"] = "New York";
```

To print out the YAML document, you can use the YAML::Node::print() method:

```Arduino
document.print(Serial);
```

This will print out the following YAML document:

```Arduino
name: John Doe
age: 30
city: New York
```

## Deep Dive:

YAML, which stands for "YAML Ain't Markup Language", was first released in 2001. It was designed to be a human-readable, cross-platform language for data storage and configuration. YAML is often compared to other markup languages such as XML and JSON, but it has the advantage of being simpler and easier to edit by hand.

There are a few alternatives to using YAML in Arduino programming, such as storing data in arrays or in text files. However, YAML offers a more structured and organized approach to data storage and is especially useful for complex data sets.

Implementing YAML in Arduino is made possible by the Arduino YAML library. This library provides functions for parsing, creating, and manipulating YAML documents. It is based on the popular LibYAML library and is regularly updated.

## See Also:

To learn more about using YAML in your Arduino projects, check out the official YAML website at https://yaml.org/. You can also visit the Arduino YAML library's GitHub page at https://github.com/greiman/YAML for more information and updates. Additionally, the Arduino forums and community are great resources for exchanging tips and ideas on implementing YAML in your projects.