---
title:                "Working with json"
html_title:           "C++ recipe: Working with json"
simple_title:         "Working with json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Working with JSON in C++ refers to parsing, creating, and manipulating data in JSON format. JSON, or JavaScript Object Notation, is a lightweight data interchange format that is commonly used in web development. Programmers use JSON to easily exchange data between different systems and languages, as it has a simple and readable syntax.

## How to:
### Parsing JSON data
To parse JSON data in C++, you can use a library like [RapidJSON](https://rapidjson.org/) or [nlohmann/json](https://github.com/nlohmann/json). Here's an example using RapidJSON:

```C++
#include <iostream>
#include "rapidjson/document.h"
#include "rapidjson/istreamwrapper.h"

using namespace rapidjson;

int main() {
    
    // Sample JSON data
    const char* json = R"(
    {
        "name": "John",
        "age": 25,
        "hobbies": ["reading", "coding", "gaming"],
        "address": {
            "street": "123 Main St.",
            "city": "New York",
            "state": "NY"
        }
    }
    )";
    
    // Parse JSON data
    Document document;
    document.Parse(json);
    
    // Get values from JSON data
    const char* name = document["name"].GetString();
    int age = document["age"].GetInt();
    
    // Output results
    std::cout << "Name: " << name << std::endl;
    std::cout << "Age: " << age << std::endl;

    // Get values from nested objects
    const char* street = document["address"]["street"].GetString();
    std::cout << "Address: " << street << std::endl;
    
    // Loop through array
    std::cout << "Hobbies: ";
    for (auto& hobby : document["hobbies"].GetArray()) {
        std::cout << hobby.GetString() << " ";
    }
    std::cout << std::endl;
    
    return 0;
}
```

Output:
```
Name: John
Age: 25
Address: 123 Main St.
Hobbies: reading coding gaming 
```
### Creating JSON data
To create JSON data in C++, you can use the same libraries as mentioned before. Here's an example using nlohmann/json:

```C++
#include <iostream>
#include "nlohmann/json.hpp"

using json = nlohmann::json;

int main() {
    
    // Create JSON object
    json data = {
        {"name", "Jane"},
        {"age", 30},
        {"hobbies", {"hiking", "painting", "cooking"}},
        {"address", {
            {"street", "456 Park Ave."},
            {"city", "Los Angeles"},
            {"state", "CA"}
        }}
    };
    
    // Output JSON data
    std::cout << data.dump(4) << std::endl;
    
    return 0;
}
```

Output:
```
{
    "name": "Jane",
    "age": 30,
    "hobbies": ["hiking", "painting", "cooking"],
    "address": {
        "street": "456 Park Ave.",
        "city": "Los Angeles",
        "state": "CA"
    }
}
```

## Deep Dive
JSON was originally created by Douglas Crockford in 2001 and has since gained widespread adoption in web development for its simplicity and compatibility with many languages. Alternatives to JSON include XML and YAML, but JSON is favored for its lightweight syntax and ease of use.

When working with JSON in C++, it's important to ensure the library you choose has good parsing and validation capabilities to avoid errors and vulnerabilities in your code. It's also helpful to have knowledge of C++ data structures and how to access nested objects and arrays, as demonstrated in the examples above.

## See Also
- [RapidJSON](https://rapidjson.org/): A fast and memory-efficient JSON parser for C++
- [nlohmann/json](https://github.com/nlohmann/json): A single header library for manipulating JSON data in C++
- [JSON.org](https://www.json.org/): The official JSON website with documentation and resources for different programming languages