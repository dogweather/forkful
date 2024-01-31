---
title:                "Working with JSON"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) is a ubiquitous data exchange format on the web. Programmers use JSON to easily pass data between servers and web clients due to its simplicity and the fact that it's natively understood by JavaScript, and thus by web browsers.

## How to:
Working with JSON in Python requires the `json` module. Here's a quick run-through:

### Parsing JSON (`json.loads`):
```Python
import json

# Imagine you've got JSON from an API
json_string = '{"name": "Alice", "age": 30, "city": "Wonderland"}'

# Parse the JSON string into a Python dictionary
person = json.loads(json_string)

print(person)
```

### Sample Output:
```Python
{'name': 'Alice', 'age': 30, 'city': 'Wonderland'}
```

### Generating JSON (`json.dumps`):
```Python
import json

# Python dictionary
person_dict = {'name': 'Alice', 'age': 30, 'city': 'Wonderland'}

# Convert the dictionary to a JSON formatted string
person_json = json.dumps(person_dict)

print(person_json)
```

### Sample Output:
```Python
'{"name": "Alice", "age": 30, "city": "Wonderland"}'
```

## Deep Dive
JSON was proposed by Douglas Crockford in the early 2000s as a part of the JavaScript language, but quickly adopted across languages due to its lightweight format. Alternatives to JSON include XML and YAML, but JSON wins for minimalism and speed. Directly in Python, JSON serializes to strings and deserializes into dictionaries or lists, making it easy to work with programmatically. Note that while JSON resembles a Python dictionary, they are not the same—you cannot use Python-specific objects and types in JSON.

## See Also
- Official JSON website: [json.org](https://www.json.org)
- Python's JSON module documentation: [Python JSON](https://docs.python.org/3/library/json.html)
- Comparison between JSON and XML: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
- Python 3.x Documentation: [python.org](https://www.python.org/doc/)
