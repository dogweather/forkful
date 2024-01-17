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

## What & Why?

Working with JSON in Python involves manipulating data in a format that is widely used for transmitting and storing data. Programmers often use it because it is lightweight, human-readable, and easy to use. It allows them to efficiently organize and transfer data between different systems.

## How to:

To work with JSON in Python, you first need to import the built-in ‘json’ module using the 'import' statement. Then you can use the loads() method to load JSON data and the dumps() method to convert Python objects into JSON strings. See the code below for an example:

```Python
import json

# Example JSON data
data = '{"name": "John", "age": 30, "city": "New York"}'

# Load JSON data into a Python dictionary
json_data = json.loads(data)

# Loop through the data
for key, value in json_data.items():
    print(key, ":", value)
```

Output:
```
name : John
age : 30
city : New York
```

## Deep Dive

JSON (JavaScript Object Notation) was introduced in 1999 as a lightweight alternative to XML. It is based on JavaScript syntax and has become a popular data format due to its simplicity and flexibility. It is often used for transmitting data between web applications and APIs.

An alternative to working with JSON in Python is using the 'simplejson' library, which provides additional functionality and better performance. 'simplejson' is a third-party library that can be easily installed through pip.

JSON is used to represent data in a key-value format that is similar to a Python dictionary. It is built on two structures: a collection of key/value pairs and an ordered list of values. Nested objects and arrays can also be represented in JSON, providing a hierarchical structure for data organization.

## See Also

- [Official Python JSON Documentation](https://docs.python.org/3/library/json.html)
- [simplejson library](https://simplejson.readthedocs.io/en/latest/)
- [JSON vs XML: The Battle Of The Lightweight Data Interchange Formats](https://www.hugoware.net/articles/json-vs-xml-%281%29)
- [Introduction to JSON](https://www.json.org/json-en.html)