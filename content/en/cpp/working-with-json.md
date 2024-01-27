---
title:                "Working with JSON"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) in C++ involves parsing and generating textual data formatted as JSON. Programmers use JSON for easy data exchange between servers and web clients, and because it’s human-readable and language-independent.

## How to:

To work with JSON in C++, you'll need to use a library like `nlohmann/json`. Here’s how you can parse and generate JSON data:

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Parsing JSON
    std::string str = R"({"name":"John", "age":30, "city":"New York"})";
    nlohmann::json parsed = nlohmann::json::parse(str);

    // Access elements
    std::cout << "Name: " << parsed["name"] << std::endl;
    std::cout << "Age: " << parsed["age"] << std::endl;

    // Generating JSON
    nlohmann::json j;
    j["name"] = "Jane";
    j["age"] = 25;
    j["city"] = "Los Angeles";

    std::cout << "Generated JSON: " << j.dump(4) << std::endl;

    return 0;
}
```

Sample Output:
```
Name: John
Age: 30
Generated JSON: {
    "age": 25,
    "city": "Los Angeles",
    "name": "Jane"
}
```

## Deep Dive:

JSON was introduced as a simple text format for data interchange and became a standard due to its simplicity and wide adoption. Alternatives like XML exist but JSON leads in web APIs due to its lower verbosity and better readability. C++ doesn’t have native JSON support, hence libraries like `nlohmann/json` are popular for handling serialization and deserialization, offering a clean API that mimics working with native data types.

## See Also:

- GitHub repository for `nlohmann/json`: https://github.com/nlohmann/json
- JSON official website for more on the format: https://www.json.org/json-en.html
- For XML handling in C++: https://pugixml.org/
- Cppreference page on string streams for advanced string handling in C++: https://en.cppreference.com/w/cpp/io/basic_stringstream
