---
title:                "Hämta en webbsida"
date:                  2024-01-20T17:44:27.845499-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta dess innehåll över internet, typiskt HTML, CSS och ibland JavaScript. Programmerare gör detta för att bearbeta informationen, skrapa data eller testa sidors tillgänglighet.

## How to:
För att ladda ner en webbsida i Lua använder vi biblioteket `socket.http`. Här är ett enkelt exempel:

```Lua
local http = require("socket.http")
local body, code, headers, status = http.request("http://example.com")

if code == 200 then
    print(body) -- Detta är innehållet på webbsidan
else
    print(status)
end
```

Du borde se det HTML-innehåll som sänds tillbaka från "http://example.com" printas ut i konsolen.

## Deep Dive:
Lua är inte byggt med webbnätverk i åtanke. Så, `socket.http` är en del av LuaSocket, en modul som gör nätverksoperationer möjliga. Alternativ inkluderar `luasec` för HTTPS, och `curl`-bindningar för mer avancerade förfrågningar. Implementationer skiljer sig åt: `socket.http` är ok för basuppgifter, men för HTTPS eller mer kontroll behövs andra bibliotek.

## Se även:
- [LuaSocket dokumentation](http://w3.impa.br/~diego/software/luasocket/http.html)
- [LuaSec GitHub-repo](https://github.com/brunoos/luasec)
- [Lua-cURL GitHub-repo](https://github.com/Lua-cURL/Lua-cURL)
