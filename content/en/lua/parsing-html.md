---
title:                "Parsing html"
html_title:           "Lua recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML is the process of analyzing and decoding HTML code to extract relevant information. Programmers often do this in order to gather data from websites or to modify and manipulate HTML structures for specific purposes.

## How to:
Parsing HTML can be easily achieved in Lua using the LuaXML library. First, install the library by running the command ```luarocks install LuaXML```. Then, import the library in your code using the line ```local xml = require("xml")```. Next, use the ```xml.load()``` function to load the HTML code into a Lua table. Finally, you can access the data in the HTML code by using the table's keys and values.

A simple example would be extracting all the links from a webpage. We can achieve this using the following code:

```Lua
local html = [[
<html>
<head>
<title>Lua Website</title>
</head>
<body>
<h1>Welcome to Lua</h1>
<a href="https://lua.org">Visit the official Lua website</a>
</body>
</html>
]]

local data = xml.load(html)

for _, link in ipairs(data.body.a) do
    print(link["@href"]) -- prints "https://lua.org"
end
```

## Deep Dive:
Parsing HTML has been a common practice in web development since the early days of the internet. In its simplest form, it involves analyzing the HTML structure and identifying different elements, tags, and attributes. This enables the manipulation, extraction, and modification of HTML code, making it an essential tool for web developers and data scientists.

While there are alternative methods for parsing HTML, Lua's LuaXML library offers a lightweight and efficient solution. In addition to retrieving specific data from HTML code, the library also supports manipulation and modification of HTML structures.

For more advanced usage, the LuaHTML library is another option that provides a more comprehensive approach to parsing HTML, including handling errors and invalid syntax.

## See Also:
- [LuaXML](https://github.com/michal-kottman/luaexpat) - Official LuaXML library
- [LuaHTML](https://github.com/tylerneylon/lua-htmlparser) - Lua library for parsing HTML
- [HTML Parsing in Lua](https://www.sqlite.org/src/doc/trunk/artifact/1d084c05d65b82bb) - In-depth discussion on HTML parsing in Lua.