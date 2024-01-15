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

## Why

JSON (JavaScript Object Notation) is a lightweight and widely used data interchange format, making it a popular choice for data storage and communication in web applications. Learning how to work with JSON in C can greatly enhance your programming skills and open up new opportunities for handling and manipulating data.

## How To

To work with JSON in C, you will need to install a library called jansson, which provides functions for encoding and decoding JSON data. You can download the latest version of jansson from their [GitHub page](https://github.com/akheron/jansson) or use your package manager to install it.

Once you have jansson installed, you can start using it in your C programs by including the "jansson.h" header file and linking the library in your compiler settings. Next, you can create JSON objects using the `json_t` data type, which represents a generic JSON value. Here's an example of how to create a JSON object with key-value pairs using jansson's `json_object()` function:

```c
json_t *json_obj = json_object();
json_object_set(json_obj, "name", json_string("John"));
json_object_set(json_obj, "age", json_integer(25));
```

In the above code, we create a JSON object `json_obj` and add two key-value pairs to it - `name` with the value "John" and `age` with the value 25. Now, let's see how we can convert this JSON object to a string using jansson's `json_dumps()` function:

```c
char *json_str = json_dumps(json_obj, JSON_ENCODE_ANY);
```

The `json_dumps()` function takes in a JSON object as its first argument and a set of options as its second argument. In this case, we use the `JSON_ENCODE_ANY` option to encode the JSON object with any type of encoding. This will return a string representation of our JSON object, which can be printed to the console or used for further processing.

Once you have finished working with your JSON data, don't forget to free the memory used by the JSON objects using jansson's `json_decref()` function:

```c
json_decref(json_obj);
free(json_str); // don't forget to free the string as well
```

## Deep Dive

Jansson offers a wide range of functions for working with JSON, including creating nested objects and arrays, retrieving and modifying values, and handling errors. You can refer to the official [jansson documentation](https://jansson.readthedocs.io/en/2.13/#) for a complete list of functions and their usage.

It's worth noting that while jansson is a powerful library for working with JSON in C, it does have some limitations, such as not being able to handle cyclic references in JSON objects. It's important to keep these limitations in mind when using jansson in your projects.

## See Also

* [Official jansson documentation](https://jansson.readthedocs.io/en/2.13/#)
* [C tutorial on JSON with jansson](https://riptutorial.com/c/example/5984/json-with-jansson)
* [GitHub page for jansson](https://github.com/akheron/jansson)