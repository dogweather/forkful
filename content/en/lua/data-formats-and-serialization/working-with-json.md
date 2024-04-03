---
date: 2024-02-03 19:03:12.466558-07:00
description: "How to: Lua does not include a built-in library for JSON processing.\
  \ Therefore, one of the popular third-party libraries is `dkjson`, which you can\
  \ easily\u2026"
lastmod: '2024-03-13T22:45:00.224622-06:00'
model: gpt-4-0125-preview
summary: Lua does not include a built-in library for JSON processing.
title: Working with JSON
weight: 38
---

## How to:
Lua does not include a built-in library for JSON processing. Therefore, one of the popular third-party libraries is `dkjson`, which you can easily use for JSON encoding and decoding. First, make sure to install `dkjson`, e.g., through LuaRocks (`luarocks install dkjson`), and then follow the examples below.

### Decoding JSON to Lua Table
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua Programmer", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Error:", err)
else
  print("Name:", luaTable.name) -- Output: Name: Lua Programmer
  print("Age:", luaTable.age) -- Output: Age: 30
  print("Languages:", table.concat(luaTable.languages, ", ")) -- Output: Languages: Lua, JavaScript
end
```

### Encoding Lua Table to JSON
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Lua Programmer",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

Sample output for encoding:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Lua Programmer"
}
```

These simple examples demonstrate how to work with JSON in Lua, making it easy to integrate Lua applications with various web technologies and external APIs. Remember, while `dkjson` is used in these examples, other libraries like `cjson` and `RapidJSON` can also be suitable alternatives depending on your project's needs.
