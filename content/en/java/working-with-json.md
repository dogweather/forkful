---
title:                "Working with json"
html_title:           "Java recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) is a common task for Java programmers. JSON is a lightweight data interchange format that is used to transmit data between a server and a web application. Programmers use JSON because it is easy to read and write, and it is supported by a wide range of programming languages and platforms.

## How to:

To work with JSON in Java, you will need to import the JSON library. Here's an example of how to import the library and read a JSON file:

```Java
import org.json.*;
import java.io.*;

// read JSON file
FileReader file = new FileReader("data.json");

// create JSON object
JSONObject obj = new JSONObject(file);

// get value of a key
String name = obj.getString("name");
```

The output of this code block will be the value of the "name" key in the JSON file.

## Deep Dive

JSON was first introduced in 2001 by Douglas Crockford as a more lightweight alternative to XML. It quickly gained popularity due to its simplicity and flexibility. Today, it is widely used in web development, mobile apps, and APIs.

There are several alternatives to JSON, such as XML, YAML, and CSV. However, JSON has become the go-to choice for many developers due to its human-readable format and ease of use. 

Working with JSON in Java involves using the JSON library, which provides a set of classes and methods for parsing, manipulating, and creating JSON objects. The library is constantly updated and maintained, making JSON a reliable and efficient choice for data interchange.

## See Also

- [Official JSON website](https://www.json.org/)
- [Java JSON library documentation](https://www.json.org/java/)
- [Comparison between JSON and XML](https://medium.com/@cstegel/comparing-json-and-xml-understanding-the-physical-differences-9ed3d1c222bc)