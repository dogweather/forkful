---
title:                "Working with YAML"
aliases:
- en/cpp/working-with-yaml.md
date:                  2024-02-03T19:03:36.080909-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML, which stands for YAML Ain't Markup Language, is a human-readable data serialization format. Programmers use it for configuration files, data dumping, and storing hierarchal data due to its readability and easy-to-understand syntax compared to XML or JSON.

## How to:

To work with YAML in C++, a popular choice is the `yaml-cpp` library. First, ensure you have `yaml-cpp` installed and properly linked to your C++ project.

**Reading a YAML file:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "Title: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

Given a `config.yaml` that looks like this:

```yaml
title: "Example YAML"
```

Running the above C++ code would produce:

```
Title: Example YAML
```

**Writing to a YAML file:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "title" << YAML::Value << "Example YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

This code will create an `output.yaml` with the content:

```yaml
title: Example YAML
```

These examples serve as a basic introduction to reading from and writing to YAML files in C++ using the `yaml-cpp` library. For more complex structures and use cases, explore the `yaml-cpp` documentation for features like sequences, tags, and more advanced serialization and deserialization techniques.
