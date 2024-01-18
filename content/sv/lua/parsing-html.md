---
title:                "Parsing html"
html_title:           "Lua: Parsing html"
simple_title:         "Parsing html"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
HTML parsing handlar om att analysera och bearbeta HTML-kod för att extrahera specifik information från en webbsida eller webbapplikation. Detta är en viktig kunskap för programmerare eftersom det möjliggör automatisering av uppgifter som att hämta specifika data från en webbsida eller att göra förändringar i en befintlig HTML-struktur.

## Så här gör du:
Här är ett enkelt exempel på hur du kan använda Lua för att parsa HTML och få ut länkarna från en webbsida:

```Lua
local html = [[
<html>
 <head>
  <title>Enkel HTML-sida</title>
 </head>
 <body>
  <h1>Det här är en enkel HTML-sida</h1>
  <ul>
   <li><a href="https://www.website1.com">Länk 1</a></li>
   <li><a href="https://www.website2.com">Länk 2</a></li>
   <li><a href="https://www.website3.com">Länk 3</a></li>
  </ul>
 </body>
</html>
]]

local links = {}
for link in html:gmatch('<a href="(.-)"') do
  table.insert(links, link)
end

print("Länkar från sidan:")
for i,link in ipairs(links) do
  print(i .. ": " .. link)
end
```

Resultatet av detta kodexempel skulle vara:
```
Länkar från sidan:
1: https://www.website1.com
2: https://www.website2.com
3: https://www.website3.com
```

## Djupdykning:
Parsing av HTML är en viktig del av webbutveckling och har funnits sedan tidigt 1990-tal. Innan detta användes ofta regelbundna uttryck för att bearbeta HTML-kod, men det kan vara ineffektivt och svårt att hantera alla möjliga variationer av HTML som kan existera. Det finns även andra språk och verktyg som kan användas för att parsa HTML, såsom Python och Beautiful Soup eller JavaScript och Cheerio.

För att implementera HTML parsing i Lua, kan man använda ett bibliotek som "LuaXML" eller "LuaHTML" som innehåller funktioner för att hantera HTML-kod och återskapa den till en Lua-struktur. Dessa bibliotek är dock inte en del av standard Lua-installationen och måste installeras separat.

## Se även:
- https://www.lua.org/
- http://www.keplerproject.org/luahtml/
- http://lua-users.org/wiki/LuaXml