---
title:                "Working with json"
html_title:           "Kotlin recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON is a widely used data format in programming. It stands for JavaScript Object Notation and is used to store and exchange data between different applications or systems. Programmers use JSON because it is easy to read, write and understand for both humans and machines. It also allows for data to be structured in a hierarchical manner, making it suitable for storing large amounts of complex data.

## How to:
Coding with JSON in Kotlin is straightforward. First, we need to add the JSON dependency to our project. In Kotlin, we can do this by adding the following code to our `build.gradle` file:

```
dependencies {
  implementation 'org.json:json:20210307'
}
```

Next, we can create a JSON object using the `JSONObject` class and add key-value pairs to it using the `put` method. For example:

```
val jsonObj = JSONObject()
jsonObj.put("name", "John")
```

We can then convert this JSON object to a string using the `toString()` method:

```
val jsonStr = jsonObj.toString()
println(jsonStr)
```

This will output: `{"name": "John"}`, which is a valid JSON string.

## Deep Dive:
JSON was first introduced in 2001 as an alternative to XML for data storage and exchange. It gained popularity due to its simplicity and lightweight nature. Other formats, such as CSV and YAML, can also be used for similar purposes, but JSON's hierarchical structure makes it easier to represent complex data.

Kotlin provides built-in support for working with JSON through the `kotlinx.serialization` library. This library allows us to easily serialize and deserialize Kotlin objects to and from JSON. However, we still need to use a third-party library, like the one mentioned in the **How to** section, to create and manipulate JSON objects.

## See Also:
- [JSON official website](https://www.json.org/)
- [Kotlinx.serialization library](https://kotlinlang.org/docs/serialization.html)
- [Comparison of JSON with other data formats](https://www.json.org/xml.html)