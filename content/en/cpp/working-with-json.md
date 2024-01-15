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

## Why
JSON (JavaScript Object Notation) is a widely used data interchange format that is easy for humans to read and write, as well as for machines to parse and generate. It has become a popular choice for data serialization in web applications, making it an essential skill for any modern programmer.

## How To
To work with JSON in C++, we first need to include the json library:
```C++
#include <json/json.h>
```
Next, we can create a JSON object and add key-value pairs to it:
```C++
Json::Value myObj;
myObj["name"] = "John";
myObj["age"] = 25;
```
We can also create nested objects and arrays:
```C++
Json::Value myNestedObj;
myNestedObj["hobbies"].append("reading");
myNestedObj["hobbies"].append("playing guitar");
myObj["personal_info"] = myNestedObj;
```
To convert the JSON object to a string, we can use the `Json::FastWriter` class:
```C++
Json::FastWriter writer;
std::string jsonString = writer.write(myObj);
```
And to parse a JSON string into a JSON object, we can use the `Json::Reader` class:
```C++
Json::Reader reader;
Json::Value parsedObj;
reader.parse(jsonString, parsedObj);

std::string name = parsedObj["name"].asString();   // "John"
int age = parsedObj["age"].asInt();                 // 25
std::string hobby1 = parsedObj["personal_info"]["hobbies"][0].asString();     // "reading"
```

## Deep Dive
The `Json::Value` class provides different methods for accessing and manipulating data based on its type. For example, `asString()` can only be used for string values, but `asInt()` can only be used for integer values. It is important to check the type of the value before using these methods to avoid unexpected errors.

The `Json::Reader` class has an `isStrictMode()` method that allows us to choose whether to strictly validate the syntax of the JSON string. This can be useful when working with data from external sources.

JSON also supports custom data types through the `Json::Value::null`, `Json::Value::boolean`, `Json::Value::array`, and `Json::Value::object` methods, allowing for more flexibility in data storage.

## See Also
- Official JSON website: https://www.json.org/
- C++ JSON library: https://github.com/open-source-parsers/jsoncpp
- Guide to using JSON in C++: https://www.json.org/example.html