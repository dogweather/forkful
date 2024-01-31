---
title:                "Parsing HTML"
date:                  2024-01-20T15:32:44.333515-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML means sifting through the maze of HTML tags to find the data you need. Programmers do it to extract info, automate web interactions, or migrate content.

## How to:

Lua isn't naturally web-savvy like Python or JavaScript, but with the `luasocket` and `luahtml` libraries, it can stride into HTML parsing territory. Let's dive in with a basic example:

```Lua
local socket = require("socket.http")
local html = require("luahtml")

-- Fetching HTML from a URL
local body, code = socket.request("http://example.com")

if code ~= 200 then
    print("Failed to load page")
    return
end

-- Parsing the HTML
local parsed_html = html.parse(body)

-- Extracting data from a specific element, say a paragraph
for _, p in ipairs(parsed_html:select("p")) do
    print(p:getcontent())
end
```

This will print the content of all paragraph tags (`<p>`) from the fetched webpage. 

## Deep Dive

HTML parsing in Lua isn't a one-stop-shop scenario. You’ve to stitch together various libraries, unlike in languages designed with web parsing in mind. Historically, Lua's been a sidekick for quick, embedded scripting in apps, not web scraping.

Alternatives? Besides `luahtml`, there's also `luascrape` and `luaxpath` for different parsing needs. There's no objectively 'best' choice—each comes with quirks you'll need to navigate.

Diving into implementation, Lua libraries generally leverage the C API for performance gains. When sifting through HTML, you'll juggle nodes and elements, each an opportunity to chase down the pesky details of web structures.

## See Also

- LuaSocket documentation: http://w3.impa.br/~diego/software/luasocket/http.html
- luahtml on GitHub for a deep dive into parsing methods: https://github.com/o-lim/luahtml
- Lua Users Wiki for community gems and troubleshooting: http://lua-users.org/wiki/
