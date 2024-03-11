---
date: 2024-02-03 19:03:17.164753-07:00
description: "YAML, short for \"YAML Ain't Markup Language,\" is a human-readable\
  \ data serialization standard that is often used for configuration files and data\
  \ exchange\u2026"
lastmod: '2024-03-11T00:14:34.088400-06:00'
model: gpt-4-0125-preview
summary: "YAML, short for \"YAML Ain't Markup Language,\" is a human-readable data\
  \ serialization standard that is often used for configuration files and data exchange\u2026"
title: Working with YAML
---

{{< edit_this_page >}}

## What & Why?

YAML, short for "YAML Ain't Markup Language," is a human-readable data serialization standard that is often used for configuration files and data exchange between languages. Programmers leverage YAML due to its simplicity and readability, making it a preferred choice for settings, diverse application configurations, or content that should be editable by non-programmers.

## How to:

Lua does not have built-in support for YAML, but you can work with YAML files by using third-party libraries such as `lyaml`. This library allows for the encoding and decoding of YAML data with Lua. First, you'll need to install `lyaml` via LuaRocks, Lua's package manager:

```bash
luarocks install lyaml
```

### Decoding YAML:

Suppose you have the following YAML content in a file named `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

You can decode this YAML file into a Lua table with the following code:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

When you run this script, it should output:

```output
host: localhost
port: 3306
username: user
password: pass
```

### Encoding YAML:

To encode Lua tables into YAML format, you use the `dump` function provided by `lyaml`. Considering you want to create a YAML representation of the following Lua table:

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

The output YAML will be:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

Following these patterns, Lua programmers can effectively manage YAML data for a variety of applications. These operations with YAML are crucial for developing versatile Lua applications that interact smoothly with other parts of a system or with other systems directly.
