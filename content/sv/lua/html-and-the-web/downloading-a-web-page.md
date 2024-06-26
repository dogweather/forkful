---
date: 2024-01-20 17:44:27.845499-07:00
description: "How to: F\xF6r att ladda ner en webbsida i Lua anv\xE4nder vi biblioteket\
  \ `socket.http`. H\xE4r \xE4r ett enkelt exempel."
lastmod: '2024-03-13T22:44:38.036939-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att ladda ner en webbsida i Lua anv\xE4nder vi biblioteket `socket.http`."
title: "H\xE4mta en webbsida"
weight: 42
---

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
