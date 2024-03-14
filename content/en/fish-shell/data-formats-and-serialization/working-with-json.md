---
date: 2024-02-03 19:03:11.371835-07:00
description: "Working with JSON in the Fish Shell involves parsing and generating\
  \ JSON data, a common task for configuring applications, API interaction, and\u2026"
lastmod: '2024-03-13T22:45:00.493218-06:00'
model: gpt-4-0125-preview
summary: "Working with JSON in the Fish Shell involves parsing and generating JSON\
  \ data, a common task for configuring applications, API interaction, and\u2026"
title: Working with JSON
---

{{< edit_this_page >}}

## What & Why?

Working with JSON in the Fish Shell involves parsing and generating JSON data, a common task for configuring applications, API interaction, and streamlining command-line workflows. Given JSON's ubiquity in web and application development, mastering its manipulation directly in the shell can significantly enhance automation and data handling efficiency for programmers.

## How to:

Fish Shell, by itself, does not have built-in utilities for parsing and generating JSON. However, it seamlessly integrates with third-party tools like `jq` for JSON processing. `jq` is a powerful and versatile command-line JSON processor that allows you to slice, filter, map, and transform structured data with a simple and expressive language.

### Parsing JSON with jq
To parse a JSON file and extract data using `jq`:

```fish
# Assuming you have a JSON file named 'data.json' with content: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# Sample output
"Fish Shell"
```

### Generating JSON with jq
Creating JSON content from shell variables or outputs:

```fish
# Create JSON object from variables
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# Sample output
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### Filtering JSON Collections
Suppose we have a JSON array of objects in a file named `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
To filter this array for only stable versions:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# Sample output
"3.1.2"
"3.4.0"
```

The examples provided demonstrate the power of integrating `jq` with Fish Shell for JSON operations. Leveraging such tools enriches the shell experience, making it a formidable environment for handling modern data formats.
