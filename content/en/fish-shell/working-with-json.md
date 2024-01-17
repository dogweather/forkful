---
title:                "Working with json"
html_title:           "Fish Shell recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a popular data format used in web development and other programming environments. It is a lightweight, human-readable format for storing and exchanging data between different systems.

Programmers use JSON because it offers a flexible way to represent data in a structured format. It is easy to read and write, making it a popular choice for data interchange between applications.

## How to:

To work with JSON in Fish Shell, you can use the built-in functions `json` and `fromjson`. These functions allow you to convert data from JSON format into a Fish Shell variable and vice versa.

Example:
```
# Convert JSON data into a Fish Shell variable
set data (json '{ "name": "John Smith", "age": 30 }')

# Access the values from the variable
echo $data[name]    # Output: John Smith
echo $data[age]     # Output: 30

# Convert a Fish Shell variable into JSON data
set info '{ "name": "Lisa Brown", "age": 25 }'
fromjson $info     # Output: `{"name": "Lisa Brown", "age": 25}`
```

## Deep Dive

JSON was first created in 2001 by Douglas Crockford and has since become popular due to its simplicity and ease of use. It is based on the JavaScript language, but can be used with many other programming languages.

An alternative to using JSON is XML (Extensible Markup Language), which was the dominant data format before JSON came along. However, JSON is more compact and easier to read, making it a preferred choice for many developers.

To work with JSON in Fish Shell, the `json` function uses the `eval` command to evaluate the data and convert it into a Fish Shell variable. On the other hand, the `fromjson` function uses the `printf` command to format the data as JSON.

## See Also

- [Fish Shell documentation for JSON](https://fishshell.com/docs/current/cmds/json.html)
- [JSON official website](https://www.json.org/)
- [XML vs JSON: The debate](https://www.upwork.com/resources/xml-vs-json)