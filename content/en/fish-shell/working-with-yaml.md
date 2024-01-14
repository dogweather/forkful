---
title:                "Fish Shell recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
If you're a Fish Shell user and working with configuration files, you've probably come across YAML. This widely-used format is great for creating human-readable data files such as configuration files, making it an essential tool for any developer.

## How To
To start working with YAML in Fish Shell, you'll first need to install the yq utility. This can be done easily through the Fish Shell plugin manager:
```
fisher add fishpkg/fish-prompt-mimisbrunnr
```
Once installed, you can use the `yq` command to perform various YAML-related tasks. Here are a few examples:
```
# View the contents of a YAML file
yq eval . file.yaml

# Convert YAML to JSON
yq eval . file.yaml -j

# Select specific values from a YAML file
yq eval '.key1.key2[2]' file.yaml
```

## Deep Dive
YAML stands for "YAML Ain't Markup Language" and is often used as an alternative to the popular markup languages such as XML and JSON. It is a human-readable data serialization standard and is often used for configuration files, but can also be used for other types of data such as invoices, messages, and logs.

One of the main advantages of using YAML is its simplicity. It uses indentation to define the structure of data, making it easy to read and understand. It also supports comments, making it easier to annotate and document your data.

Additionally, YAML supports various data types such as strings, numbers, Booleans, and even complex structures like lists and dictionaries. This flexibility makes it a powerful tool for handling different types of data.

## See Also
- [Official YAML Website](https://yaml.org/)
- [Fish Shell Documentation](https://fishshell.com/docs/current/)
- [yq Utility Documentation](https://mikefarah.gitbook.io/yq/)