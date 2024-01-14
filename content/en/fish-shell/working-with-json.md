---
title:                "Fish Shell recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Why
JSON (JavaScript Object Notation) is a lightweight data interchange format commonly used for exchanging data between a server and a web application. It is easy to read and write, making it a popular choice for APIs. Learning how to work with JSON in Fish Shell can be extremely useful for developers, as it allows for seamless integration with different systems and data sources.

## How To
Coding with JSON in Fish Shell can be achieved with a few simple commands. Let's take a look at some examples:

```
# Creating a JSON object
set -l json '{ "name": "John", "age": 30, "interests": ["coding", "hiking"] }'
```

```
# Accessing values in the object
echo $json.name # Output: John
echo $json.interests[1] # Output: hiking
```

```
# Converting JSON to a Fish Shell array
set -q -g json | read -lo values; json_parse --as-array $values > my_array
```

```
# Writing a JSON file
json_write my_file < my_array
```

These are just a few examples of how you can work with JSON in Fish Shell. Experiment with different commands and see what you can come up with!

## Deep Dive
One of the key benefits of JSON is its simplicity, which makes it easy to understand and work with. When working with more complex JSON data, you may need to use the `jq` command, which allows you to parse and manipulate JSON data in a variety of ways. This can be especially useful when working with deeply nested data or when needing to filter specific values.

In addition, Fish Shell also has a built-in `jsonlint` command, which can help ensure that your JSON code is valid and properly formatted.

## See Also
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [JSON Specification](https://www.json.org/json-en.html)
- [Jq Documentation](https://stedolan.github.io/jq/)
- [JSONLint](https://jsonlint.com/)