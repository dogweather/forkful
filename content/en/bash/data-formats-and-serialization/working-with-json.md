---
date: 2024-02-03 19:03:04.210805-07:00
description: "Working with JSON in Bash programming involves parsing, extracting,\
  \ and manipulating JSON data directly from the command line. Programmers often do\
  \ this\u2026"
lastmod: '2024-03-13T22:45:00.263329-06:00'
model: gpt-4-0125-preview
summary: Working with JSON in Bash programming involves parsing, extracting, and manipulating
  JSON data directly from the command line.
title: Working with JSON
weight: 38
---

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
