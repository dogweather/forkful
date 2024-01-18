---
title:                "Working with yaml"
html_title:           "Lua recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML is a way to organize and store data in a more human-readable format compared to other data formats like JSON or XML. Programmers often use YAML as a configuration file for their applications or as a way to store data that needs to be easily readable and modifiable.

## How to:

Using YAML in Lua is simple and straightforward. First, we need to require the `yaml` module:

```Lua
local yaml = require "yaml"
```

To encode a Lua table into YAML, we can use the `yaml.dump` function:

```Lua
local data = {
    name = "John Doe",
    age = 25,
    hobbies = {"coding", "reading", "hiking"}
}

print(yaml.dump(data))
```

Output:

```Lua
---
name: John Doe
age: 25
hobbies:
  - coding
  - reading
  - hiking
```

To decode a YAML string into a Lua table, we can use the `yaml.load` function:

```Lua
local yaml_string = [[
    ---
    name: Jane Smith
    age: 30
    hobbies:
      - painting
      - travelling
]]

local data = yaml.load(yaml_string)

print(data.name)
print(data.age)
print(data.hobbies[1])
```

Output:

```Lua
Jane Smith
30
painting
```

## Deep Dive:

YAML stands for "YAML Ain't Markup Language" and was created as a human-friendly data serialization language. It was first released in 2001 and has since gained popularity due to its simplicity and readability. Alternative formats such as JSON and XML have their own strengths and use cases, but YAML offers a balance between being easy to read and write for humans, and easily parsable by computers.

Implementations of YAML in Lua, such as the `yaml` module, use the libyaml library for encoding and decoding. This ensures fast and efficient processing of YAML data.

## See Also:

- [YAML website](https://yaml.org/)
- [LuaRocks page for yaml module](https://luarocks.org/modules/genericlama/yaml)