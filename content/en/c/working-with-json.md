---
title:                "Working with json"
html_title:           "C recipe: Working with json"
simple_title:         "Working with json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-json.md"
---

{{< edit_this_page >}}

### What & Why?
JSON, or JavaScript Object Notation, is a lightweight data interchange format that is commonly used in web development. It allows programmers to easily transfer and manipulate data between applications and systems, making it a popular choice among developers. Simply put, working with JSON is a way to organize, store, and transmit data in a readable and efficient format.

### How to:
To work with JSON in C, you'll need to use a library called "json-c" which provides functions for parsing, creating, and manipulating JSON objects. Here's a simple example of how to use it:

```C
// First, include the necessary headers
#include <stdio.h>
#include <string.h>
#include <json-c/json.h>

// Then, create a JSON object and add data to it
struct json_object *person = json_object_new_object();
json_object_object_add(person, "name", json_object_new_string("John Smith"));
json_object_object_add(person, "age", json_object_new_int(30));

// To retrieve data from the JSON object, use the appropriate functions
char *name;
int age;
json_object_object_get_ex(person, "name", &name);
json_object_object_get_ex(person, "age", &age);

// Finally, print the data
printf("Name: %s\nAge: %d\n", name, age);
```

The output of this code would be:
```
Name: John Smith
Age: 30
```
Now you can see how easy it is to work with JSON in C!

### Deep Dive
JSON was first introduced in 1999 by Douglas Crockford and has since become a widely adopted data format. It was created as an alternative to XML, which was considered too verbose and complex for many applications.

There are a few alternatives to JSON, such as YAML and XML, but JSON is preferred for its simple and flexible structure. It is also a more lightweight option, making it ideal for use in web development where efficiency is important.

The json-c library is an implementation of the JSON data format in C. It provides functions for creating, parsing, and manipulating JSON objects, as well as converting them to and from strings. The library is actively maintained and can be easily installed through package managers or downloaded from the official website.

### See Also
For more information and examples of using json-c, check out the following resources:

- [json-c official website](https://github.com/json-c/json-c)
- [JSON tutorial on W3Schools](https://www.w3schools.com/js/js_json_intro.asp)
- [C programming language on GeeksforGeeks](https://www.geeksforgeeks.org/c-programming-language/)