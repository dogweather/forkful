---
title:                "Working with yaml"
html_title:           "Python recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-yaml.md"
---

{{< edit_this_page >}}

# Working with YAML in Python

## What & Why?
YAML (short for "YAML Ain't Markup Language") is a data serialization format ideal for configuration files and data exchange between programming languages with similar data types. Programmers use it for its human-readability, simplicity, and compatibility with languages like Python.

## How to:
We will use the Python module `PyYAML` to work with YAML data. You can comfortably install it with pip:

```python
pip install pyyaml
```

Here's how to load YAML data:

```python
import yaml

yaml_data = """
name: John Doe
age: 30
"""

data = yaml.load(yaml_data, Loader=yaml.FullLoader)
print(data)
```

Output:

```python
{'name': 'John Doe', 'age': 30}
```

And, here's how to write data into a YAML file:

```python
import yaml

data = {
    'name': 'John Doe',
    'age': 30
}

with open('data.yaml', 'w') as file:
    yaml.dump(data, file)
```

## Deep Dive
YAML, first proposed by Clark Evans in 2001, was designed to be easily interfaced with scripting languages. It is often compared to JSON for its readability, though YAML offers a more robust feature set.

JSON is a subset of YAML, making JSON data compatible with a YAML parser. However, YAML offers features like including comments and self-referencing, which JSON does not.

Working with YAML in Python is straightforward due to the native data types in Python being a subset of YAML’s. Beware of Python's libraries' subtle differences when working with YAML files. For example, PyYAML follows the YAML 1.1 specification and supports custom tags and anchors, whereas other libraries like ruamel.yaml follow YAML 1.2, being more suitable for preserving comments and other details in YAML files.

## See Also
- PyYAML Documentation: http://pyyaml.org/wiki/PyYAMLDocumentation
- YAML Specifications: http://yaml.org/spec/1.2/spec.html
- Python & YAML turorial: https://zetcode.com/python/yaml/