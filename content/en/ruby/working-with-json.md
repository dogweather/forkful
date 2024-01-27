---
title:                "Working with JSON"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, or JavaScript Object Notation, is a lightweight data interchange format. Programmers use JSON to store and exchange data because it's easy to read and write for humans and simple to parse for machines.

## How to:
In Ruby, you can work with JSON using the built-in 'json' library. To use it, just require 'json' at the top of your code.

```Ruby
require 'json'

# Convert a Ruby hash to a JSON string
user = { name: "John Doe", email: "john.doe@example.com" }
json_string = user.to_json
puts json_string
# Output: {"name":"John Doe","email":"john.doe@example.com"}

# Parse a JSON string to a Ruby hash
json_string = '{"name":"Jane Doe","email":"jane.doe@example.com"}'
parsed_data = JSON.parse(json_string)
puts parsed_data["name"]
# Output: Jane Doe
```

## Deep Dive
JSON originated in the early 2000s. Douglas Crockford, its promoter, sought to make data sharing between server and client in web applications simpler compared to XML.

Alternatives to JSON include XML and YAML, though JSON's ease of use and widespread support make it a go-to format. JSON parsing in Ruby is efficient because the 'json' library is built on native extensions written in C, which speeds up the parsing significantly.

## See Also
- JSON specification and info site: [JSON.org](https://www.json.org/json-en.html)
- Comparison of JSON and XML: [XML vs JSON](https://www.w3schools.com/js/js_json_xml.asp)
