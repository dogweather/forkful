---
date: 2024-02-03 19:03:25.452076-07:00
description: "Working with JSON (JavaScript Object Notation) involves parsing JSON\
  \ formatted strings into Python objects and vice versa. This is crucial for web\
  \ and API\u2026"
lastmod: '2024-03-11T00:14:33.577150-06:00'
model: gpt-4-0125-preview
summary: "Working with JSON (JavaScript Object Notation) involves parsing JSON formatted\
  \ strings into Python objects and vice versa. This is crucial for web and API\u2026"
title: Working with JSON
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) involves parsing JSON formatted strings into Python objects and vice versa. This is crucial for web and API development as JSON is the lingua franca for exchanging data between servers and clients.

## How to:

Python's built-in `json` library simplifies the process of encoding (converting Python objects to JSON) and decoding (converting JSON to Python objects). Here's how you can use it:

### Encoding Python objects to JSON:

```python
import json

data = {
    "name": "John Doe",
    "age": 30,
    "isEmployee": True,
    "addresses": [
        {"city": "New York", "zipCode": "10001"},
        {"city": "San Francisco", "zipCode": "94016"}
    ]
}

json_string = json.dumps(data, indent=4)
print(json_string)
```

**Output:**

```json
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
```

### Decoding JSON to Python objects:

```python
json_string = '''
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
'''

data = json.loads(json_string)
print(data)
```

**Output:**

```python
{
    'name': 'John Doe', 
    'age': 30, 
    'isEmployee': True, 
    'addresses': [
        {'city': 'New York', 'zipCode': '10001'}, 
        {'city': 'San Francisco', 'zipCode': '94016'}
    ]
}
```

### Working with third-party libraries:

For complex JSON handling, such as schema validation or parsing JSON files directly from URLs, libraries like `requests` for HTTP requests and `jsonschema` for validation can be helpful.

#### Example with `requests` to parse JSON from a URL:

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

This snippet fetches JSON data from a given URL and directly converts it into a Python object.

#### Using `jsonschema` to validate JSON:

First, install the library via pip:

```bash
pip install jsonschema
```

Then, use it as follows:

```python
from jsonschema import validate
import jsonschema

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "number"},
        "isEmployee": {"type": "boolean"},
    },
    "required": ["name", "age", "isEmployee"]
}

# Assuming `data` is a dictionary obtained from JSON decoding
try:
    validate(instance=data, schema=schema)
    print("Valid JSON data.")
except jsonschema.exceptions.ValidationError as err:
    print("Validation error:", err)
```

This example validates your Python dictionary (obtained from decoded JSON data) against a predefined schema, ensuring that the data conforms to expected formats and types.
