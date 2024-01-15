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

## Why

YAML is a lightweight and human-readable data format that is commonly used in programming for data serialization and configuration. It is often preferred over other formats, such as JSON, due to its simplicity and ease of use.

## How To

To work with YAML in Python, we first need to install the `pyyaml` package using pip:

```
pip install pyyaml
```

Once the package is installed, we can import it into our Python code:

```
import yaml
```

To read a YAML file, we can use the `load()` function:

```
with open('data.yml') as f:
    data = yaml.load(f, Loader=yaml.FullLoader)
```

This will read the data from the YAML file and store it in the variable `data` as a dictionary. We can then access the data by using standard dictionary notation, for example:

```
print(data['name']) # prints the value of the 'name' key
```

To write data to a YAML file, we can use the `dump()` function:

```
my_data = {'foo': 'bar', 'baz': [1, 2, 3]}
with open('output.yml', 'w') as f:
    yaml.dump(my_data, f)
```

This will create a new YAML file called `output.yml` and write the data from the `my_data` dictionary to it in YAML format.

## Deep Dive

YAML stands for "YAML Ain't Markup Language" and is designed to be a human-friendly data serialization format. It uses indentation and whitespace to represent data structures, making it easy to read and write for humans.

YAML supports a variety of data types, including strings, lists, dictionaries, and even custom objects. It also allows for comments and references, making it flexible for a variety of use cases.

One important thing to note when working with YAML in Python is the use of the `yaml.SafeLoader` or `yaml.FullLoader` to prevent arbitrary code execution. By using these safer loaders, we can ensure that the data in our YAML file is not accidentally executed as code.

## See Also

- [Official PyYAML Documentation](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Real Python Guide to YAML in Python](https://realpython.com/python-yaml/)
- [Python YAML Tutorial from TutorialsPoint](https://www.tutorialspoint.com/python/yaml_processing_in_python.htm)