---
date: 2024-01-20 18:00:22.875044-07:00
description: 'Vorgehensweise: So sendet man eine simple HTTP GET-Anfrage in Lua.'
lastmod: '2024-03-13T22:44:54.012315-06:00'
model: gpt-4-1106-preview
summary: So sendet man eine simple HTTP GET-Anfrage in Lua.
title: Einen HTTP-Request senden
weight: 44
---

## Vorgehensweise:
So sendet man eine simple HTTP GET-Anfrage in Lua:

```Lua
local http = require("socket.http")
local body, code, headers, status = http.request("http://example.com")
print(status)  -- Sollte 'HTTP/1.1 200 OK' zurückgeben, wenn alles glatt lief.
print(body)    -- Der Inhalt der Webseite.
```

Eine POST-Anfrage gestaltet sich ähnlich, erfordert aber zusätzliche Parameter:

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

local response_body = {}
local payload = "param1=value1&param2=value2"

local result, code, response_headers, status = http.request{
    url = "http://example.com/api",
    method = "POST",
    headers = {
        ["Content-Type"] = "application/x-www-form-urlencoded",
        ["Content-Length"] = tostring(#payload)
    },
    source = ltn12.source.string(payload),
    sink = ltn12.sink.table(response_body)
}

print(status)
print(table.concat(response_body))
```

Output sollte den Status der Anfrage und die Antwort vom Server zeigen.

## Vertiefung:
Historischer Kontext: Lua entstand 1993 in Brasilien und ist bekannt für seine Einfachheit sowie Effizienz. Es war nicht für Webanwendungen konzipiert, aber Erweiterungen wie LuaSocket erlauben HTTP-Kommunikation.

Alternativen: Andere HTTP-Bibliotheken für Lua sind `curl` oder `HTTPClient`. Für komplexe Anwendungen mag man eine vollwertige Web-Entwicklungsframeworks wie `OpenResty` betrachten.

Implementierungsdetails: `LuaSocket` nutzt die Socket-Programmierung, um mit HTTP-Servern zu kommunizieren. POST-Anfragen senden Daten im Rumpf, GET-Anfragen hängen Parameter an die URL.

## Siehe Auch:
- LuaSocket Dokumentation: http://w3.impa.br/~diego/software/luasocket/
- Lua Programming Language Offizielle Seite: https://www.lua.org/
- Lua HTTP Modul bei LuaRocks: https://luarocks.org/modules/luarocks/lua-http
