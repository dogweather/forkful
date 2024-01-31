---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML is a data serialization format easy for humans to read and write. Programmers use it for config files, data exchange between languages, and structured data storage.

## How to:

Lua doesn't have built-in support for YAML, but you can use a library like `lyaml`. Install it using `luarocks install lyaml`. Here’s how to parse YAML:

```Lua
local lyaml = require('lyaml')

-- Sample YAML data as a string
local yaml_data = [[
- name: John Doe
  age: 29
- name: Jane Smith
  age: 42
]]

-- Parsing YAML string to Lua table
local parsed_data = lyaml.load(yaml_data)

-- Accessing data
for i, person in ipairs(parsed_data) do
  print(person.name, person.age)
end
```

Sample output:
```
John Doe 29
Jane Smith 42
```

Now let’s generate some YAML from a Lua table:

```Lua
local lyaml = require('lyaml')

-- Sample Lua table
local people = {
  { name = "John Doe", age = 29 },
  { name = "Jane Smith", age = 42 }
}

-- Generating YAML from Lua table
local yaml_output = lyaml.dump(people)

print(yaml_output)
```

Sample YAML output:
```
- age: 29
  name: John Doe
- age: 42
  name: Jane Smith
```

## Deep Dive

YAML, which stands for "YAML Ain't Markup Language", emerged in early 2000s as a user-friendly data serialization standard. It's less verbose than XML and JSON which makes it popular for configuration files. Alternatives include JSON, XML, and TOML. Lua implementation mostly relies on external libraries like `lyaml` which uses libYAML under the hood for parsing and emitting YAML. When using YAML with Lua, remember that tables don't have an inherent order, so lists in YAML become arrays, but dictionaries (key-value pairs) may not preserve order.

## See Also

- YAML official website: https://yaml.org
- `lyaml` library on GitHub: https://github.com/gvvaughan/lyaml
- LuaRocks package for `lyaml`: https://luarocks.org/modules/gvvaughan/lyaml
