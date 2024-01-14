---
title:                "Python recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why 
Python is a popular programming language because of its versatility and ease of use. One of the many great features of Python is its ability to work with different file formats, including YAML. This makes it a valuable tool for developers and data scientists who need to manipulate and analyze YAML files.

## How To 
Working with YAML in Python is simple and straightforward. Let's take a look at some coding examples and sample output to see just how easy it is.

Firstly, we need to import the necessary library, PyYAML, which can be done with the following code: 

```Python
import yaml
```

Next, we can load a YAML file using the `load()` method. Let's say we have a YAML file named `pets.yaml`, which contains a list of different pets and their attributes. We can load this file and store its contents in a variable called `pet_list` by using the following code: 

```Python
with open('pets.yaml') as f:
    pet_list = yaml.load(f, Loader=yaml.FullLoader)
```

Now, we can easily access and work with the data in the `pet_list` variable. For example, if we want to print all the names of the pets in the list, we can do so with a for loop as shown below:

```Python
for pet in pet_list:
    print(pet['name'])
```

This will give us the following output:

```bash
Fluffy
Buddy
Spot
Luna
```

We can also modify the data in the YAML file and save it using the `dump()` method. For example, if we want to add a new pet to the list, we can do so with the following code:

```Python
pet_list.append({'name': 'Max', 'age': 3, 'type': 'dog'})
with open('pets.yaml', 'w') as f:
    yaml.dump(pet_list, f)
```

This will add the new pet to the `pets.yaml` file and save the changes.

## Deep Dive 
YAML, which stands for "YAML Ain't Markup Language", is a human-readable data serialization language. It is commonly used for configuration files, but it can also be used for data storage and exchange.

YAML files follow a hierarchical structure and use indentation to define the relationships between data. It uses key-value pairs and supports various data types such as strings, numbers, lists, and dictionaries.

In Python, the PyYAML library provides functions for loading and dumping YAML files. It also offers different loaders for more flexible and secure handling of YAML files.

A unique feature of YAML is its ability to use anchors and aliases, which allow for the reuse of data by referencing it in other parts of the file. This makes YAML files more concise and easier to maintain.

## See Also 
- Official PyYAML documentation: https://pyyaml.org/wiki/PyYAMLDocumentation
- YAML official website: https://yaml.org/
- YAML tutorial for beginners: https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/