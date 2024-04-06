---
date: 2024-01-20 18:02:00.107207-07:00
description: "Hur g\xF6r man: Grundl\xE4ggande autentisering \xE4r en del av HTTP-standarden\
  \ som har anv\xE4nts sedan b\xF6rjan av webben f\xF6r att skydda inneh\xE5ll. Det\
  \ \xE4r enkel att\u2026"
lastmod: '2024-04-05T22:50:52.341818-06:00'
model: gpt-4-1106-preview
summary: "Grundl\xE4ggande autentisering \xE4r en del av HTTP-standarden som har anv\xE4\
  nts sedan b\xF6rjan av webben f\xF6r att skydda inneh\xE5ll."
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

## Hur gör man:
```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

local username = "användare"
local password = "lösenord"
local auth = "Basic " .. (require("mime").b64(username .. ":" .. password))

local response = {}
local body, code, headers, status = http.request {
  url = "http://exempelsida.se/data",
  method = "GET",
  headers = {
    ["Authorization"] = auth
  },
  sink = ltn12.sink.table(response)
}

if code == 200 then
  print(table.concat(response))
else
  print(status)
end
```
Exempelutdata:
```
{"message": "Hej! Autentisering lyckades."}
```

## Fördjupning
Grundläggande autentisering är en del av HTTP-standarden som har använts sedan början av webben för att skydda innehåll. Det är enkel att implementera men inte det säkraste, eftersom användarnamn och lösenord skickas i klartext kodat i Base64. Moderna alternativ inkluderar OAuth och JWT. Implementeringsdetaljer är viktiga - använd HTTPS för att undvika oönskad avlyssning och se till att hantera lösenord varsamt.

## Se även
- [socket.http dokumentation](http://w3.impa.br/~diego/software/luasocket/http.html)
- [RFC 7617, 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617)
- [LuaSec för HTTPS-kommunikation](https://github.com/brunoos/luasec/wiki)
- [JWT i Lua](https://github.com/SkyLothar/lua-resty-jwt)
