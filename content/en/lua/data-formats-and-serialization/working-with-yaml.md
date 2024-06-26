---
date: 2024-02-03 19:03:17.164753-07:00
description: "How to: Lua does not have built-in support for YAML, but you can work\
  \ with YAML files by using third-party libraries such as `lyaml`. This library allows\u2026"
lastmod: '2024-03-13T22:45:00.223731-06:00'
model: gpt-4-0125-preview
summary: Lua does not have built-in support for YAML, but you can work with YAML files
  by using third-party libraries such as `lyaml`.
title: Working with YAML
weight: 41
---

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
