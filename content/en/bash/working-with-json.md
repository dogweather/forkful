---
title:                "Working with JSON"
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON in Bash involves parsing and generating JSON-formatted data directly from the command line. Programmers do this for configuration management, API interaction, and data interchange between services due to JSON's ubiquity across platforms and languages.

## How to:

```Bash
# Parsing JSON using 'jq':
echo '{"name": "John", "age": 31, "city": "New York"}' | jq '.name'
# Output: "John"

# Generating JSON using 'jq':
echo '{}' | jq --arg name "John" --arg city "New York" '. | .name=$name | .city=$city'
# Output: {"name":"John","city":"New York"}

# Reading JSON file and extracting data:
jq '.users[] | select(.id == "123")' users.json
# Assuming users.json contains the relevant data structure.
```

## Deep Dive

JSON (JavaScript Object Notation) was formalized in the early 2000s and quickly became a standard for data interchange. In a Bash context, `jq` emerged as a robust tool for JSON processing which provides a DSL (domain-specific language) for querying and manipulating JSON data. Alternatives include `jshon` and `jo`. Working with JSON in Bash typically involves using external tools like these because Bash does not have built-in JSON parsing capabilities.

## See Also

- `jq` Manual: https://stedolan.github.io/jq/manual/
- Wikipedia article on JSON: https://en.wikipedia.org/wiki/JSON
- Bash Scripting Guide: https://www.gnu.org/software/bash/manual/