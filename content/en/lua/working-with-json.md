---
title:                "Working with json"
html_title:           "Lua recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Working with JSON in Lua involves using a library to handle data in the JSON format, which is a popular way of storing and exchanging data in web applications. Programmers do this to easily parse and manipulate data from APIs and databases, as well as to create and send data in a format that can be easily understood by other programs.

## How to:
To work with JSON in Lua, you first need to install a library. One of the most commonly used is "lua-cjson", which can be installed using the LuaRocks package manager with the command `luarocks install lua-cjson`. Then, you can use the library in your code by requiring it at the top with `local cjson = require("cjson")`.

To convert a Lua table into a JSON string, you can use the `encode()` function:
```Lua
local data = {name = "John", age = 25, hobbies = {"coding", "reading"}}
local json_str = cjson.encode(data)
print(json_str)
-- Output: {"name": "John", "age": 25, "hobbies": ["coding", "reading"]}
```

Similarly, you can convert a JSON string into a Lua table with the `decode()` function:
```Lua
local json_str = '{"name": "Jane", "age": 30, "hobbies": ["cooking", "hiking"]}'
local data = cjson.decode(json_str)
print(data.name)
-- Output: Jane
```

## Deep Dive:
JSON, short for JavaScript Object Notation, was created in the early 2000s as a lightweight data interchange format. It is based on a subset of the JavaScript programming language and is human-readable, making it easy for both humans and machines to understand.

In Lua, there are multiple libraries available for working with JSON, including "lua-cjson", "lua-json", and "rxi-json". Each has its own advantages and disadvantages, so it is important to choose the one that best fits your needs.

Implementations of JSON libraries can vary in terms of performance, memory usage, and error handling. It's important to consider these factors when selecting a library for your project.

## See Also:
- [Official Lua Documentation](https://www.lua.org/docs.html)
- [JSON Official Website](https://www.json.org/)
- [LuaRocks](https://luarocks.org/) package manager
- [lua-cjson Documentation](https://www.kyne.com.au/~mark/software/lua-cjson-doc/)
- [lua-json GitHub Repository](https://github.com/harningt/lua-json)
- [rxi-json GitHub Repository](https://github.com/rxi/json.lua)