---
date: 2024-01-25 03:39:58.864514-07:00
description: "Working with XML involves parsing and manipulating XML documents using\
  \ code. Programmers do this to read, write, and modify data in a structured, portable\u2026"
lastmod: '2024-03-13T22:45:00.227212-06:00'
model: gpt-4-1106-preview
summary: "Working with XML involves parsing and manipulating XML documents using code.\
  \ Programmers do this to read, write, and modify data in a structured, portable\u2026"
title: Working with XML
---

{{< edit_this_page >}}

## What & Why?
Working with XML involves parsing and manipulating XML documents using code. Programmers do this to read, write, and modify data in a structured, portable format that’s widely used for data exchange and storage.

## How to:
Lua doesn't include native XML parsing, but there are libraries like LuaXML and xml2lua that get the job done. Here's a quick look at parsing XML with xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programming in Lua</book></root>]])

print(handler.root.book._attr.id)  -- Outputs: 123
print(handler.root.book[1])        -- Outputs: Programming in Lua
```

For writing XML, here's a mini example using LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programming in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Outputs: <root><book id="123">Programming in Lua</book></root>
```

## Deep Dive
XML, short for Extensible Markup Language, has been a standard in data representation and exchange since the mid-90s. It gives structure to data and is both human-readable and machine-parseable.

While JSON and YAML are now favored for their simplicity, XML remains prevalent in many enterprise and legacy systems. In Lua, native XML handling isn't built-in because Lua is designed to be small and extensible through modules.

XML libraries for Lua, like LuaXML, xml2lua, and others, bridge this gap. LuaXML provides a lightweight XML reader and writer, while xml2lua uses an event-driven approach similar to SAX parsers. These libraries are usually implemented in pure Lua for portability, while some might rely on C for performance.

When it comes to performance and memory usage, Lua's XML libraries may not be as fast as those in languages with native support. However, for most use-cases in Lua, especially in game development or scripting for embedded systems, these libraries do a fine job without overloading the system.

## See Also
- LuaXML on GitHub: https://github.com/LuaDist/luaxml
- xml2lua on GitHub: https://github.com/manoelcampos/xml2lua
- Lua.org's list of libraries: https://lua-users.org/wiki/LibrariesAndBindings
