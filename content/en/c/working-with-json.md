---
title:                "C recipe: Working with json"
simple_title:         "Working with json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-json.md"
---

{{< edit_this_page >}}

## Why

In today's digital world, data is constantly being exchanged between different systems and applications. This data comes in various formats, with one of the most popular being JSON (JavaScript Object Notation). JSON is a lightweight, human-readable data interchange format that is widely used for transmitting data over the internet. With its simplicity and flexibility, it has become a popular choice for developers in various programming languages, including C. In this blog post, we will dive into the basics of working with JSON in C programming.

## How To

Before we dive into coding, let's quickly go over the basics of JSON. JSON consists of key-value pairs, with the data enclosed in curly braces. A key is a string that identifies the value, and the value can be a string, number, boolean, array, or object. Here's an example of a JSON object:

```C
{
  "name": "John Doe",
  "age": 25,
  "hobbies": ["reading", "gaming", "coding"]
}
```

To start working with JSON in C, we need to first include the JSON-C library. You can download the library from its [official website](https://github.com/json-c/json-c). Once the library is installed, we can begin parsing JSON data using the `json_object` struct. Here's a simple code snippet to illustrate this:

```C
#include <stdio.h>
#include <json-c/json.h>

int main() {
    // declare variables
    struct json_object *obj;
    struct json_object *name;
    struct json_object *age;
    struct json_object *hobbies;

    // parse JSON data
    obj = json_tokener_parse(
        "{\"name\": \"John Doe\", \"age\": 25, \"hobbies\": [\"reading\", \"gaming\", \"coding\"]}"
    );

    // get values using keys
    json_object_object_get_ex(obj, "name", &name);
    json_object_object_get_ex(obj, "age", &age);
    json_object_object_get_ex(obj, "hobbies", &hobbies);

    // print values
    printf("Name: %s\n", json_object_get_string(name));
    printf("Age: %d\n", json_object_get_int(age));

    // loop through array values and print
    int len = json_object_array_length(hobbies);
    for (int i = 0; i < len; i++) {
        printf("Hobby %d: %s\n", i+1, json_object_get_string(json_object_array_get_idx(hobbies, i)));
    }

    return 0;
}
```

Running this code will produce the following output:

```
Name: John Doe
Age: 25
Hobby 1: reading
Hobby 2: gaming
Hobby 3: coding
```

Now that we have a basic understanding of working with JSON in C, let's take a deep dive into some more advanced concepts.

## Deep Dive

One important thing to note when working with JSON in C is the memory management aspect. The JSON-C library provides functions such as `json_object_put()` and `json_object_array_add()` to manage memory for us. It's important to use these functions properly to avoid memory leaks. Also, when creating a new JSON object, we can specify whether the object should automatically free memory for its values when it is destroyed or not. This can be done by setting the `JSON_C_OBJECT_ADD_KEY_IS_NEW` flag. For example:

```C
struct json_object *obj = json_object_new_object();
json_object_set_new(obj, "key1", json_object_new_string("value1"));
json_object_set_new(obj, "key2", json_object_new_string("value2"));
json_object_set_new(obj, "key3", json_object_new_string("value3"));
json_object_set_new_flag(obj, "key4", JSON_C_OBJECT_ADD_KEY_IS_NEW, json_object_new_object());
```

Another useful function provided by the JSON-C library is `json_object_to_json_string()` which converts a JSON object into a string. This is helpful when you need to send JSON data over a network or save it in a file. Additionally, the library also allows for parsing JSON data from a string or file using the `json_tokener_parse()` and `json_object_from_file()` functions respectively.

See Also:

- [JSON-C library documentation](https://json-c.github.io/json-c/)
- [JSON-C Github repository](https://github.com/json-c/json-c)
- [JSON Wikipedia page](https://en.wikipedia.org/wiki/JSON)