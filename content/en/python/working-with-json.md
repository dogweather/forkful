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

# Working with JSON in Python: A Handy Guide 

## What & Why?

JavaScript Object Notation (JSON) is a popular data interchange format. Python developers use JSON for sending and receiving data from a web server, and storing complex data structures in an easily-readable format.

## How to:

Let's dive straight into some codes. Firstly, we need to import the `json` library in Python. 

```Python
import json
```

This is how you can convert a Python object into a JSON string (`json.dumps`):

```Python
import json
person = {"name": "John", "age": 30, "city": "New York"}
person_json = json.dumps(person)
print(person_json)
```

**Output:**

```Python
'{"name": "John", "age": 30, "city": "New York"}'
```

To convert a JSON string into a Python object (`json.loads`):

```Python
import json
person_json = '{"name": "John", "age": 30, "city": "New York"}'
person = json.loads(person_json)
print(person)
```

**Output:**

```Python
{'name': 'John', 'age': 30, 'city': 'New York'}
```

## Deep Dive

Originating from JavaScript in the early 2000s, JSON has now become a well-adopted standard format for data interchange in many languages, including Python. From saving Python objects, delivering server responses to storing complex data structures, JSON's usefulness is hard to deny.

What if I say, JSON isn't the only game in town? Alternatives such as YAML, XML, and Protobuf also exist, each with its own advantages.

JSON operates in Python with the built-in json module. It uses simple methods to convert between JSON strings and Python's native data types (like `dict`, `list`, `str`, `int`, etc.). This direct match makes handling JSON data quite seamless in Python.

## See Also

- Python's official [json documentation](https://docs.python.org/3/library/json.html)
- Douglas Crockford's [original JSON specification site](https://www.json.org/json-en.html)
- Introductory guide to [Python's data structures](https://docs.python.org/3/tutorial/datastructures.html)