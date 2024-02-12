---
title:                "Working with JSON"
aliases:
- /en/bash/working-with-json/
date:                  2024-02-03T19:03:04.210805-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Working with JSON in Bash programming involves parsing, extracting, and manipulating JSON data directly from the command line. Programmers often do this to seamlessly integrate shell scripts with web APIs and modern data interchange formats, making Bash scripting more powerful and relevant in a JSON-heavy ecosystem.

## How to:
Bash itself lacks built-in JSON parsing capabilities, but `jq` is a powerful command-line JSON processor that fills this gap. Here's how to use it:

**Reading a JSON file:**

Sample `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

To read and extract the name from the JSON file:
```bash
jq '.name' data.json
```
Output:
```
"Jane Doe"
```

**Modifying JSON data:**

To update the city to "Los Angeles" and write back to the file:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**Parsing JSON from a variable:**

If you have JSON in a Bash variable, `jq` can still process it:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
Output:
```
"John Doe"
```

**Working with arrays:**

Given an array of items in JSON:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

To extract the second item (indexing starts at 0):
```bash
jq '.items[1]' data.json
```
Output:
```
"banana"
```

For more complex operations and filtering, `jq` has a comprehensive manual and tutorials available online, making it a versatile tool for all your Bash/JSON needs.
