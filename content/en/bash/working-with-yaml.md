---
title:                "Working with yaml"
html_title:           "Bash recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML is a way for programmers to manage and organize data in a human-readable format. It is often used for configuring applications and systems, and is especially popular in the DevOps and automation world. YAML files use a simple indentation format, making it easy for anyone to read.

## How to:

To work with YAML in Bash, you will need to have the "yq" tool installed. You can do this by using the package manager for your operating system. Once installed, you can start working with YAML files by using the "yq" command.

```
# Example command to get a value from a YAML file using "yq"
yq eval '.key' file.yaml
```

```
# Example output from above command
value
```

To modify a YAML file, you can use the "yq" command with the "-i" flag. This will edit the file in place instead of just displaying the output.

```
# Example command to modify a key in a YAML file using "yq"
yq eval -i '.key = "new value"' file.yaml
```

## Deep Dive:

YAML was first introduced in 2001 and stands for "YAML Ain't Markup Language." It was designed to be easy for humans to read and write, while still being easily parsed by machines. YAML is often seen as an alternative to XML or JSON for configuration files.

An alternative tool for working with YAML in Bash is "shyaml." It offers similar functionality to "yq" but with slightly different syntax. It is worth exploring both options to see which one works best for your needs.

Behind the scenes, "yq" uses the "jq" library to parse and manipulate the YAML data. This means that "yq" is essentially a wrapper for "jq" commands specifically designed for YAML files. For those familiar with "jq," this can be a useful insight.

## See Also:

- [yq GitHub repo](https://github.com/mikefarah/yq)
- [shyaml GitHub repo](https://github.com/martinblech/xml2json)
- [YAML official website](https://yaml.org/)