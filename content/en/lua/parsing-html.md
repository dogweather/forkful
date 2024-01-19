---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML is a way to break down and understand an HTML document's structure and content. Programmers parse HTML to extract, manipulate or interact with data within web pages.

## How to:

To parse HTML using Lua, there’s a convenient library named LuaHTML. If it’s not installed yet, Then use Luarocks package manager to install:

```Lua
luarocks install luahtml
```

Afterwards, you can parse HTML like so:

```Lua
local luahtml = require("luahtml")

local html = [[
<html>
  <body>
    <h1>Hello, world!</h1>
  </body>
</html>
]]

local root = luahtml.parse(html)

root:query_selector("h1")[1]:get_text() -- This will output 'Hello world!'
```

The function `query_selector()` will grab the elements which match the CSS selector you've given, returning them in a Lua table.

## Deep Dive

HTML parsing has many uses including web scraping, testing web applications, and building web crawlers. HTML parsing started just after the creation of HTML language itself back in 1990 because technically HTML is meant to be parsed for rendering it in the browser. 

In the early 2000s, with surge in dynamic content delivery on the web and increase in custom analytics requirements the usage of HTML parsing outside of browser also exploded.

Besides LuaHTML, Lua programmers have other library options for parsing HTML like lua-expat (based on the well known Expat XML parser) or htmlparser.lua. The choice depends on your specific requirements and the compatibility with your current Lua version.

LuaHTML shines in its ability to carry out HTML parsing using simple and familiar CSS selectors, making it a very intuitive choice for your HTML parsing needs.

## See Also:

- More about LuaHTML: https://github.com/siffiejoe/lua-luahtml
- HTML parsing with lua-expat: https://matthewwild.co.uk/projects/luaexpat/
- Lua's htmlparser library: https://github.com/msva/lua-htmlparser
- Official Lua website: https://www.lua.org
- LuaRocks: https://luarocks.org/