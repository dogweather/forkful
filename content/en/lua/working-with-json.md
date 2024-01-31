---
title:                "Working with JSON"
date:                  2024-01-19
simple_title:         "Working with JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is used to store and transport data. Programmers use JSON because it's lightweight, easy for humans to read and write, and easy for machines to parse and generate.

## How to:

Let's parse some JSON.

```lua
-- Ensure you have the 'dkjson' module or another JSON library.
local dkjson = require 'dkjson'

local jsonString = '{"name":"John", "age":30, "city":"New York"}'

-- Parse JSON string into a Lua table.
local person, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
    print("Error:", err)
else
    print(person.name)  -- Output: John
end

-- Convert Lua table to JSON string.
local personTable = { name = "Jane", age = 25, city = "Los Angeles" }
local jsonOutput = dkjson.encode(personTable)
print(jsonOutput)  -- Output: {"age":25,"city":"Los Angeles","name":"Jane"}
```

Now let's handle arrays.

```lua
local jsonArrayString = '[{"name":"John"}, {"name":"Jane"}]'

-- Parse JSON string with an array into Lua table.
local peopleArray, _, err = dkjson.decode(jsonArrayString)
if err then
    print("Error:", err)
else
    for i, person in ipairs(peopleArray) do
        print(person.name)  -- Output: John\nJane
    end
end
```

## Deep Dive

JSON became the de facto standard for APIs, outgrowing XML because it's less verbose. There are alternatives like YAML, which is even more readable but not as widely used in APIs. In Lua, there's no native JSON support, so you need a library like 'dkjson' or 'cjson'. Lua implementation details include handling type differences, like arrays and objects, and converting between Lua's `nil` and JSON's `null`.

## See Also

- [dkjson library on GitHub](https://github.com/LuaDist/dkjson)
- [JSON official website](https://www.json.org/json-en.html)
- [Programming in Lua (first edition)](https://www.lua.org/pil/contents.html) for learning Lua basics.
