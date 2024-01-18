---
title:                "Ladda ner en webbsida"
html_title:           "Lua: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida är helt enkelt processen att hämta information från en webbserver och visa den på din dator eller enhet. Programerare gör detta för att kunna hämta data från externa källor och integrera den i sina applikationer.

## Så här gör du:
För att ladda ner en webbsida med hjälp av Lua kan du använda funktionen "get" från LuaSocket-biblioteket. Nedan är ett exempel på kod som hämtar innehållet på en webbsida och skriver ut det på skärmen:

```
local http = require("socket.http")
local body, code, headers = http.request("https://www.lua.org/")
print(body)
```

Output:
```
<html>
<head>
<title>Welcome to Lua</title>
...

## Djupdykning:
Ladda ner en webbsida med hjälp av Lua är inte nytt, det har funnits sedan LuaSocket-biblioteket släpptes 2001. Alternativ till LuaSocket inkluderar Luv och Lua-cURL. När det gäller implementation så används Socket-API för att kommunicera med webbservern och HTTP-protokollet för att överföra data.

## Se även:
- [LuaSocket](https://github.com/diegonehab/luasocket)
- [Luv](https://github.com/luvit/luv)
- [Lua-cURL](https://github.com/Lua-cURL/Lua-cURLv)