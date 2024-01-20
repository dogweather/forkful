---
title:                "Working with json"
html_title:           "Bash recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) is a common task for programmers, as it is a widely used data format for exchanging information between applications and systems. JSON is lightweight, easily readable by both humans and machines, and supported by many programming languages. It is especially popular in web development for handling API responses.

## How to:
To work with JSON in Bash, we can use the built-in tool `jq`. `jq` is a powerful command-line JSON processor that allows us to parse, filter, and manipulate JSON data.

To get started, we first need to install `jq` on our system. Depending on your operating system, this can be done through a package manager or by downloading and installing the binary directly.

Once `jq` is installed, we can start using it in our Bash scripts. Here are some examples of how to use `jq`:

- To extract a specific value from a JSON file:
```Bash
jq '.key' file.json
```
This will return the value of the key "key" in the JSON file.

- To filter JSON data based on a condition:
```Bash
jq '.[] | select(.age > 25)' file.json
```
This will return all entries in the file where the "age" value is greater than 25.

- To modify JSON data:
```Bash
jq '.key = "new value"' file.json
```
This will update the value of "key" in the JSON file to "new value".

For more advanced usage, such as handling arrays and objects, check out the [official documentation](https://stedolan.github.io/jq/manual/).

## Deep Dive:
JSON was first introduced in 2001 and has since become a popular data format due to its simplicity and flexibility. Before JSON, XML was the dominant format for data exchange, but its complex syntax and large file size made it less desirable.

Although `jq` is the most commonly used tool for working with JSON in Bash, there are alternative solutions such as `json.sh` and `jsonv`. These tools may have different features and syntax, so it's important to choose the one that fits your needs best.

Under the hood, `jq` uses an internal DSL (Domain-Specific Language) for working with JSON, making it more efficient and flexible than using regular shell commands. It also supports various output formats, making it easier to integrate with other tools and APIs.

## See Also:
- [Official `jq` documentation](https://stedolan.github.io/jq/)
- [Online JSON validator and formatter](https://jsonformatter.curiousconcept.com/)