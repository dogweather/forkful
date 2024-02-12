---
title:                "Skicka en http-förfrågan"
aliases: - /sv/lua/sending-an-http-request.md
date:                  2024-01-20T18:00:25.961742-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran är hur din kod pratar med andra servrar över internet. Programmerare gör detta för att hämta data, skicka information eller interagera med webbtjänster.

## Så här gör du:
Lua har inte inbyggt stöd för HTTP, så vi använder `socket.http` från LuaSocket biblioteket. Först, installera LuaSocket:

```shell
luarocks install luasocket
```

Sedan en enkel GET-begäran:

```lua
local http = require("socket.http")
local body, code = http.request("http://httpbin.org/get")

if code == 200 then
    print("Hämtat innehåll:")
    print(body)
else
    print("HTTP-begäran misslyckades med kod: " .. code)
end
```

Och för en POST-begäran med data:

```lua
local http = require("socket.http")
local ltn12 = require("ltn12")

local response = {}
local body, code = http.request{
    url = "http://httpbin.org/post",
    method = "POST",
    headers = {
        ["Content-Type"] = "application/x-www-form-urlencoded"
    },
    source = ltn12.source.string("nyckel1=värde1&nyckel2=värde2"),
    sink = ltn12.sink.table(response)
}

if code == 200 then
    print("Serverns svar:")
    print(table.concat(response))
else
    print("HTTP-begäran misslyckades med kod: " .. code)
end
```

## Fördjupning
Före LuaSocket var alternativet att implementera en socketkommunikation från grunden via Lua:s inbyggda filhanteringsfunktioner, vilket var både svårare och mindre säkert. LuaSocket gör det enkelt att hantera HTTP-begäran genom att abstrahera bort alla komplexiteter som att hantera TCP/IP-protokoll.

Utöver HTTP erbjuder LuaSocket även SMTP, FTP och andra nätverksprotokoll. För HTTPS-stöd behöver man oftast en tilläggsmodul - LuaSec. Det finns också modernare alternativ som luajit-request, som är baserat på LuaJIT.

När det kommer till implementationen av en HTTP-begäran är det viktigt att hantera timeouts och fel korrekt för att undvika att tillämpningen hänger sig eller kraschar.

## Se även
- LuaSocket dokumentation: http://w3.impa.br/~diego/software/luasocket/
- LuaSec för HTTPS: https://github.com/brunoos/luasec/wiki
- `luajit-request` för ett alternativ: https://github.com/hishamhm/luajit-request
