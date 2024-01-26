---
title:                "Working with YAML"
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML involves parsing and generating data in the human-friendly YAML Ain't Markup Language. Programmers use it for config files, data serialization, and application settings due to its readability and simplicity.

## How to:

YAML support isn't built-in C++. You'll need a library like `yaml-cpp`. Here's how to parse a simple YAML file:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("config.yaml");
    YAML::Node config = YAML::Load(file);
    
    std::string username = config["user"]["name"].as<std::string>();
    int age = config["user"]["age"].as<int>();
    
    std::cout << "Name: " << username << ", Age: " << age << std::endl;
    return 0;
}
```

Assuming `config.yaml` is:
```
user:
  name: John Doe
  age: 30
```

Output:
```
Name: John Doe, Age: 30
```

## Deep Dive

YAML was first introduced in 2001 as a human-readable data serialization standard. While JSON and XML are common alternatives, YAML's minimal syntax has made it popular for configuration files. Libraries like `yaml-cpp` handle parsing and emitting YAML data, representing it in structures like maps and sequences, similar to JSON objects and arrays.

## See Also

- YAML 1.2 Specification: https://yaml.org/spec/1.2/spec.html
- yaml-cpp GitHub Repository: https://github.com/jbeder/yaml-cpp
- An Introduction to YAML: https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started
