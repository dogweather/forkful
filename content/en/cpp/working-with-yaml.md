---
title:                "Working with yaml"
html_title:           "C++ recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with YAML is a common practice among programmers, especially those dealing with configuration files and data serialization. YAML, or "YAML Ain't Markup Language", is a lightweight data format that is human-readable and easy to use. Programmers use YAML because it offers a simple and intuitive way to store and share structured data, making it useful for various applications such as web development, data interchange, and configuration management.

## How to:
To work with YAML in your C++ code, you will need a YAML library. One popular option is the "yaml-cpp" library, which can be easily installed and included in your project. Here's a simple example of reading data from a YAML file and printing it out:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main() {
    YAML::Node config = YAML::LoadFile("config.yml"); //loads data from "config.yml" into a YAML::Node
    std::cout << "Username: " << config["username"] << std::endl;
    std::cout << "Password: " << config["password"] << std::endl;
}
```

Sample output:

```
Username: johndoe
Password: ********
```

Working with more complex data types, such as lists and nested structures, is also straightforward. Here's an example of defining a list of names in a YAML file and printing them out in reverse order:

```
#names.yml
- John
- Jane
- Bob
```

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main() {
    YAML::Node names = YAML::LoadFile("names.yml");
    std::cout << "Names in reverse order: " << std::endl;
    for(int i = names.size(); i > 0; i--) {
        std::cout << names[i-1].as<std::string>() << std::endl;
    }
}
```

Sample output:

```
Names in reverse order:
Bob
Jane
John
```

## Deep Dive:
YAML was first introduced in 2001 and was initially intended as a better alternative to XML for data serialization. Its simple and human-readable syntax quickly gained popularity and it is now commonly used in various programming languages, including C++. YAML's structure is based on key-value pairs, making it easy for developers to understand and work with.

While YAML has gained widespread usage, there are alternatives such as JSON and TOML which offer similar advantages. JSON is a popular data format in web development, while TOML is gaining popularity for its simplicity and support for hierarchical data structures.

In terms of implementation, the "yaml-cpp" library for C++ is open-source and actively maintained. It supports the full YAML 1.2 specification and provides a comprehensive API for working with YAML data. There are also other libraries and tools available for working with YAML in C++, such as "YAMLcpp", "LibYAML", and "YAML-cpp-parser".

## See Also:
- [YAML 1.2 Specification](https://yaml.org/spec/1.2/spec.html)
- [yaml-cpp Library](https://github.com/jbeder/yaml-cpp)
- [YAML examples](https://learnxinyminutes.com/docs/yaml/)