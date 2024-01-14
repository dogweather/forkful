---
title:                "C++ recipe: Working with json"
simple_title:         "Working with json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON, short for JavaScript Object Notation, has become a staple format for data interchange in modern programming. It's lightweight, easy to read, and widely supported by various programming languages, making it a popular choice for storing and transferring data. In this blog post, we'll delve into the world of C++ and see how we can work with JSON data.

## How To

To work with JSON in C++, we'll be using a popular library called "nlohmann/json". To start, we need to include the library in our code:

```C++
#include <iostream>
#include "json.hpp"

using json = nlohmann::json;
```

Next, we'll create a JSON object and populate it with some data, and then serialize it using the `dump()` function:

```C++
json data = {
  {"name", "John"},
  {"age", 25},
  {"hobbies", {"coding", "reading", "gaming"}}
};

std::cout << data.dump() << std::endl;
```

The output of the above code would be:

```C++
{
  "name": "John",
  "age": 25,
  "hobbies": ["coding", "reading", "gaming"]
}
```

We can also access and modify data in a JSON object using familiar syntax:

```C++
data["age"] = 26;
```

We can also parse a JSON string to create a JSON object:

```C++
std::string str = "{\"name\": \"Jane\", \"age\": 30}";

json parsed = json::parse(str);
```

The library also provides many other useful functions such as checking if a key exists, iterating through objects, and more. You can check out the full documentation for more details.

## Deep Dive

The "nlohmann/json" library is built on top of C++11 features, making it easy to use and efficient. It also provides a variety of ways to interact with JSON data, including using iterators, the `at()` function, and more. Additionally, the library has built-in support for conversions to and from different data types, making it convenient to work with existing data structures in your code.

JSON also supports much more than just basic data types. It allows for nesting objects and arrays, as well as storing more complex data such as dates, binary data, and even custom objects. This makes it a versatile format for storing and transferring data in various applications.

## See Also

- [nlohmann/json Documentation](https://github.com/nlohmann/json/blob/develop/README.md)
- [JSON Tutorial](https://www.json.org/json-en.html)