---
title:                "Python recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-json.md"
---

{{< edit_this_page >}}

## Why
JSON (JavaScript Object Notation) is a popular data interchange format that has become a common way of storing and exchanging data in web applications. It is a lightweight, human-readable format that is easy for both humans and machines to understand. JSON is widely used because it is flexible, efficient, and well-supported by many programming languages and frameworks.

## How To
To work with JSON in Python, you will need to import the built-in `json` module. This module provides functions for encoding and decoding JSON data. Let’s take a look at a simple example:

```Python
import json

# Create a JSON object
person = {
  "name": "John",
  "age": 28,
  "city": "New York"
}

# Encode the JSON object to a string
person_json = json.dumps(person)

# Print the result
print(person_json)

# Decode the string back to a JSON object
person_dict = json.loads(person_json)

# Print the result
print(person_dict)
```

Running this code will output the following:

```Python
{"name": "John", "age": 28, "city": "New York"}
{'name': 'John', 'age': 28, 'city': 'New York'}
```

You can also load JSON data from a file using the `load()` function, and save JSON data to a file using the `dump()` function. These functions accept a file object as a parameter. Here’s an example:

```Python
# Load JSON data from a file
with open('data.json') as f:
  data = json.load(f)

# Print the result
print(data)

# Save JSON data to a file
with open('results.json', 'w') as f:
  json.dump(results, f)
```

## Deep Dive
JSON supports six data types: string, number, boolean, null, object, and array. The object and array data types are used for storing complex data. Objects are enclosed in curly brackets `{}`, while arrays are enclosed in square brackets `[]`.

Keys in a JSON object must be strings, while values can be any of the supported data types. It is important to follow proper JSON syntax when working with JSON data. For example, keys and strings must be enclosed in double quotes `"`, and values must be separated by a comma `,`.

Additionally, JSON allows for nesting of objects and arrays. This means you can have objects within objects, and arrays within objects or arrays. To access data in a nested structure, you can use dot notation (e.g. `person.name`) or bracket notation (e.g. `person["name"]`) depending on the structure of your data.

## See Also
- Official Python JSON Documentation: https://docs.python.org/3/library/json.html
- JSON Tutorial: https://www.json.org/json-en.html
- JSON Playground: https://jsoneditoronline.org/