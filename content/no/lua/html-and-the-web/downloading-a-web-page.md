---
date: 2024-01-20 17:44:27.431995-07:00
description: "Slik gj\xF8r du: F\xF8r `socket.http` var den greie m\xE5ten \xE5 laste\
  \ ned nettsider i Lua, brukte programmere ofte kommandolinje-verkt\xF8y som `curl`\
  \ via\u2026"
lastmod: '2024-04-05T22:50:54.932081-06:00'
model: gpt-4-1106-preview
summary: "F\xF8r `socket.http` var den greie m\xE5ten \xE5 laste ned nettsider i Lua,\
  \ brukte programmere ofte kommandolinje-verkt\xF8y som `curl` via `os.execute`."
title: Nedlasting av en nettside
weight: 42
---

## Slik gjør du:
```Lua
-- Last ned en nettside med Lua

-- Sørg for å ha 'socket.http' installert
local http = require("socket.http")
local body, code = http.request("http://www.eksempel.no")

if code == 200 then
    print(body)
else
    print("Kunne ikke laste ned siden, statuskode: " .. code)
end
```
Sample Output:
```
<!DOCTYPE html>
<html>
<head>
    <title>Din Tittel Her</title>
</head>
<body>
    Innholdet på siden...
</body>
</html>
```

## Dypdykk
Før `socket.http` var den greie måten å laste ned nettsider i Lua, brukte programmere ofte kommandolinje-verktøy som `curl` via `os.execute`. Alternativer i Lua verden inkluderer `luasocket`, for enkel tilgang til TCP og UDP sockets, og `lua-requests` for en mere HTTP-sentrisk tilnærming som etterligner Python `requests` modulen. Ved nedlasting håndterer `socket.http` omformingshåndtering og feilkoder, men du må behandle omadresseringer og avansert HTTP funksjonalitet for hånd eller med ekstra biblioteker.

## Se også
- LuaSocket dokumentasjon: http://w3.impa.br/~diego/software/luasocket/
- LuaSec for HTTPS støtte: https://github.com/brunoos/luasec/wiki
- 'lua-requests' for et alternativ som etterligner Python requests: https://github.com/JakobGreen/lua-requests
