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

## Why

JSON, or JavaScript Object Notation, is a widely used data interchange format that allows for easy parsing and sharing of data between different systems. It is commonly used in web development, API design, and data storage. Working with JSON in Bash can make data manipulation and automation processes much easier.

## How To

To work with JSON in Bash, you will first need to install the `jq` tool. This tool allows you to parse and manipulate JSON data directly from the command line. Once installed, you can use it like this:

```
# Parse a JSON file and display a specific key
jq '.key' file.json 

# Create a new JSON object and add a key-value pair
jq '. + {new_key: "value"}' file.json

# Filter an array in a JSON file based on a key value
jq '.array | map(select(.key == "value"))' file.json 
```

These are just a few examples of what you can do with `jq`. It's a powerful tool with many more features and options for working with JSON data. Keep in mind that the syntax for `jq` can be a bit different from other Bash commands, so it may take some time to get used to.

## Deep Dive

JSON is composed of key-value pairs that are separated by commas and enclosed in curly braces. The value can be a string, number, boolean, array, or another object. The `jq` tool allows you to traverse and extract specific values or manipulate the entire JSON object.

One of the most useful functions of `jq` is its ability to filter and extract data based on specific keys or values. This is especially helpful when dealing with large JSON files with nested objects or arrays. You can also use conditionals and loops within `jq` to further manipulate the data.

There are also many other tools and libraries available for working with JSON in Bash, such as `json_pp` for pretty printing, `json_merge` for merging multiple JSON files, and `jsonlint` for validating the syntax of JSON data.

## See Also

For more information about working with JSON in Bash, check out these links:
- Official `jq` documentation: https://stedolan.github.io/jq/
- 10 `jq` Tricks for Handling JSON: https://hackernoon.com/10-jq-tricks-for-handling-json-5c477536b1f5
- JSON manipulation in Bash using `jq`: https://opensource.com/article/17/2/json-manipulation-in-bash-using-jq