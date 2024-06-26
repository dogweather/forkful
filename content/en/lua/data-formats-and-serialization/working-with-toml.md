---
date: 2024-01-25 03:39:54.987680-07:00
description: 'How to: First, make sure your Lua environment has a TOML parser. We''ll
  use `lua-toml` for this example.'
lastmod: '2024-03-13T22:45:00.226366-06:00'
model: gpt-4-1106-preview
summary: First, make sure your Lua environment has a TOML parser.
title: Working with TOML
weight: 39
---

## How to:
First, make sure your Lua environment has a TOML parser. We'll use `lua-toml` for this example.

```Lua
local toml = require("toml")

-- Parse TOML string
local toml_data = [[
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML Example"

-- Generate TOML string
local table_data = {
  title = "TOML Example",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Sample Output:
```
TOML Example
```

## Deep Dive
TOML was created by Tom Preston-Werner in 2013 as an alternative to other data serialization languages like XML and YAML, offering a more straightforward format to represent configuration data. While JSON is ubiquitous, its syntax can be cumbersome for config files. TOML shines with a clearer syntax for humans, resembling .ini files but with nesting capabilities and data types.

Alternatives to TOML include JSON, YAML, and XML. However, TOML is specifically designed for config and is arguably simpler than YAML, more readable than JSON for config purposes, and less verbose than XML.

Implementing TOML handling in Lua generally requires a third-party library. Performance and features can vary, from basic parsing to full serialization support. When dealing with large config files or frequent reading/writing operations, consider the library's performance and compliance with the latest TOML version.

## See Also
- TOML Specification: https://toml.io/en/
- `lua-toml` library: https://github.com/jonstoler/lua-toml
- Comparison of Data Serialization Formats: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
