---
title:                "Working with JSON"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a data format used for representing structured data. Programmers use JSON because it's easy to read and write for humans, and easy to parse and generate for computers.

## How to:

```Fish Shell
# Parse JSON from a string using `jq`
echo '{"name": "Fish", "type": "Shell"}' | jq '.'

# Get value of a specific key
echo '{"name": "Fish", "type": "Shell"}' | jq '.name'

# Output:
# "Fish"

# Update a value and output new JSON string
echo '{"name": "Fish", "type": "Shell"}' | jq '.type = "Command Line Interface"'

# Pretty-print JSON from a file
cat config.json | jq '.'
```

## Deep Dive

JSON, standardized in the early 2000s, has its roots in JavaScript's object literals. It quickly replaced XML for many tasks due to its lightweight syntax and direct mapping to data structures. Alternatives like YAML and TOML exist but JSON's ubiquity makes it a default choice in many scenarios. Working with JSON in Fish requires tools like `jq` because Fish itself isn't designed for heavy data manipulation. Historically, Unix shells use external tools for specific tasks, and Fish follows this philosophy.

## See Also

- The `jq` Manual: https://stedolan.github.io/jq/manual/
- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- JSON Specification: https://www.json.org/json-en.html
