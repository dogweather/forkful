---
title:                "C++ recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

YAML (YAML Ain't Markup Language) has become a popular data serialization format due to its ease of use and readability for both humans and machines. It is commonly used in web development, configuration files, and data storage. Working with YAML can make your code more efficient and organized, leading to a smoother development process.

## How To

To start working with YAML in your C++ code, you will need a YAML library. One popular option is "yaml-cpp", which can be easily installed using the package manager in your operating system. Once installed, include the library in your code by using ```#include "yaml-cpp/yaml.h"```.

To parse a YAML file, use the ```YAML::LoadFile()``` function, passing in the path to your YAML file as the argument. This will return a ```Node``` object, which represents the entire YAML document.

```C++
#include "yaml-cpp/yaml.h"

int main() {

    // Load YAML file
    YAML::Node doc = YAML::LoadFile("example.yaml");

    // Access data from the YAML document
    std::string name = doc["name"].as<std::string>();
    int age = doc["age"].as<int>();
    std::vector<std::string> hobbies = doc["hobbies"].as<std::vector<std::string>>();

    // Print the data
    std::cout << "Name: " << name << std::endl;
    std::cout << "Age: " << age << std::endl;
    std::cout << "Hobbies: ";
    for (const auto& hobby : hobbies) {
        std::cout << hobby << ", ";
    }
    std::cout << std::endl;

    return 0;
}
```

**Output:**
```
Name: John
Age: 25
Hobbies: reading, hiking, cooking,
```

To create a new YAML document, use the ```Node``` class to construct a hierarchy of nodes and then output the document using the ```YAML::Emitter``` class.

```C++
#include "yaml-cpp/yaml.h"

int main() {

    // Create YAML document hierarchy
    YAML::Node root;
    root["name"] = "Jane";
    root["age"] = 30;
    root["language"] = "C++";
    root["skills"]["project_management"] = true;
    root["skills"]["programming"] = true;
    root["skills"]["team_management"] = false;

    // Output the YAML document
    YAML::Emitter out;
    out << root;
    std::cout << out.c_str() << std::endl;

    return 0;
}
```

**Output:**
```
name: Jane
age: 30
language: C++
skills:
  team_management: false
  project_management: true
  programming: true
```

## Deep Dive

YAML provides a clean and concise syntax for representing data, making it easy to read and understand for both developers and non-technical users. It can also handle complex data structures, such as arrays and nested objects, making it a versatile choice for data storage and transfer.

One interesting aspect of working with YAML is the ability to define custom tags, which allow you to extend the functionality of the language. These tags can be used to define custom data types or modify how the data is processed. For example, you can use a custom tag to automatically convert strings to lowercase or uppercase, or to parse certain data as a date or time object.

Additionally, YAML supports comments, making it a great choice for storing configuration settings. Comments can be used to provide context and explanations for the data stored in the document, making it easier to maintain and modify in the future.

## See Also
- [YAML website](https://yaml.org/)
- [yaml-cpp library](https://github.com/jbeder/yaml-cpp)
- [YAML Tutorial](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)