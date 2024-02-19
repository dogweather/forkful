---
aliases:
- /en/cpp/working-with-json/
date: 2024-02-03 19:03:09.069503-07:00
description: "JSON (JavaScript Object Notation) is a lightweight format for storing\
  \ and transporting data, making it an excellent medium for data interchange between\u2026"
lastmod: 2024-02-18 23:09:11.376182
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) is a lightweight format for storing and\
  \ transporting data, making it an excellent medium for data interchange between\u2026"
title: Working with JSON
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a lightweight format for storing and transporting data, making it an excellent medium for data interchange between servers and web applications. Programmers use JSON due to its easy readability by humans and straightforward parsability by machines, especially when working on applications requiring data interchange over the internet or configuration settings.

## How to:

In C++, there's no native support for JSON, but third-party libraries like nlohmann/json make it straightforward. Here's how to use it for basic tasks:

First, ensure you have the library installed. If you're using a package manager like vcpkg or Conan, you can easily add `nlohmann/json` to your project.

### Parsing JSON from a string

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON data as a string
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Parse JSON string
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Accessing data
    std::cout << "Name: " << jsonObject["name"] << "\n"
              << "Age: " << jsonObject["age"] << "\n"
              << "City: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Sample output:**

```
Name: John
Age: 30
City: New York
```

### Generating JSON

Creating JSON data is just as straightforward; you simply assign values to a `nlohmann::json` object.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Creating a JSON object
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Convert JSON object to string and print
    std::string jsonString = jsonObject.dump(4); // Argument 4 for pretty-printing
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Sample output:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

These examples demonstrate the core functionality for working with JSON in C++ using the `nlohmann/json` library. With these basics, you can parse and generate JSON for various applications, from configuration files to data interchange in networked applications.
