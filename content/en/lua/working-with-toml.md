---
title:                "Working with TOML"
date:                  2024-01-25T03:39:54.987680-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with TOML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/working-with-toml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with TOML involves parsing and generating TOML (Tom's Obvious, Minimal Language) data with Lua. Programmers utilize TOML for config files due to its readability and simple syntax that easily translates to a data structure.

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
