---
title:                "Working with json"
html_title:           "Python recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-json.md"
---

{{< edit_this_page >}}

## Why

If you're a programmer, chances are you've encountered JSON (JavaScript Object Notation) at some point in your career. JSON has become a popular format for storing and exchanging data due to its simplicity and compatibility with various programming languages. Knowing how to work with JSON can open up many opportunities for data manipulation and integration in your projects.

## How To

First, let's import the `json` library in our Python code:
```Python
import json
```

To read a JSON file, we can use the `json.load()` function and pass in the file object:
```Python
with open("data.json") as f:
    data = json.load(f)
```
Now we can access the data from the JSON file using the familiar dictionary syntax:
```Python
print(data["name"]) # expects "John"
print(data["age"]) # expects 28
```

To write data in a JSON format, we can use the `json.dump()` function and pass in the data and file object:
```Python
data = {
    "name": "Emily",
    "age": 25
}

with open("new_data.json", "w") as f:
    json.dump(data, f)
```
The above code will create a new JSON file with the given data.

## Deep Dive

JSON consists of key-value pairs, making it easy to work with in Python as it maps directly to dictionaries. However, if you need more control over the data, you can use the `json.loads()` function to deserialize a JSON string into a Python object. Similarly, you can use the `json.dumps()` function to serialize a Python object into a JSON string.

JSON also supports arrays, which can be represented in Python as lists. To access data from nested objects or arrays in JSON, we can use dot notation or square brackets respectively.

It's important to note that JSON only supports certain data types, such as strings, numbers, booleans, and null values. Custom objects or functions cannot be serialized into a JSON format.

## See Also

Check out the official Python documentation for more on working with JSON: 
- [JSON in Python](https://docs.python.org/3/library/json.html) 
- [Official JSON website](https://www.json.org/json-en.html)