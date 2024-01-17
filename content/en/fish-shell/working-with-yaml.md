---
title:                "Working with yaml"
html_title:           "Fish Shell recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with YAML is a common task for programmers as it allows for easy configuration and data storage in a human-readable format. YAML stands for "YAML Ain't Markup Language" and is often used in place of XML due to its simplicity and readability.

## How to:
Coding examples in the ```Fish Shell``` can greatly simplify working with YAML. For example, to print the contents of a YAML file, you can use the following command:
```
cat sample.yaml
```

This will display the YAML file's contents in the terminal. You can also use the ```YAMLCPP``` Fish Shell library to interact with YAML in your script. Here's a sample code that creates a YAML file and adds key-value pairs to it:
```
yaml create sample.yaml
yaml set sample.yaml key value
```

The resulting YAML file will look like this:
```yaml  
key: value  
```

## Deep Dive
YAML was first designed in 2001 as a more user-friendly alternative to XML. It stands out with its easy indentation-based syntax and lack of closing tags. Some alternative data formats include JSON and TOML, but YAML remains a popular choice due to its balance of readability and complexity.

The ```YAMLCPP``` library for Fish Shell is based on the YAML-CPP library written in C++. It provides a powerful set of functions for manipulating YAML data, such as ```yaml get```, ```yaml insert```, and ```yaml delete```. This makes working with YAML in the Fish Shell even easier.

## See Also
To learn more about working with YAML in the Fish Shell, check out the official documentation at https://fishshell.com/docs/current/index.html. You can also find more information on the ```YAMLCPP``` library at https://github.com/jbeder/yaml-cpp.