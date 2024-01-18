---
title:                "Sända en http-begäran"
html_title:           "Lua: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi pratar om att skicka en HTTP-request, menar vi att vi skickar en förfrågan till en server för att få information. Det är en viktig del av programmering för att hämta och hantera data från internet.

## Hur man:
### Enkel HTTP-förfrågan med Lua socket:
```Lua
-- Ladda in Lua socket biblioteket
local socket = require("socket")

-- Ange URL och port för förfrågan
local url = "www.example.com"
local port = 80

-- Öppna en anslutning till servern
local connection = socket.tcp()
connection:connect(url, port)

-- Ange HTTP-förfrågan
local request = "GET / HTTP/1.1\r\nHost: " .. url .. "\r\n\r\n"

-- Skicka förfrågan och spara svaret
connection:send(request)
local response = connection:receive("*a")

-- Skriv ut svaret
print(response)

-- Stäng anslutning
connection:close()
```
Exempelutgång:
`<!DOCTYPE html><html><head><title>Example Domain</title>...` (fortsätter med lång HTML-kod)

## Deep Dive:
Att skicka HTTP-förfrågningar har en lång historia och är en viktig del av internet. Det finns olika vägar att skicka förfrågningar och sätt att hantera svaren, men en populär metod idag är att använda sig av bibliotek som Lua socket eller LuaHTTP. Implementationen kan variera, men grundprincipen är densamma - skapa en anslutning till servern och skicka en förfrågan.

## Se även:
- [Lua socket](http://w3.impa.br/~diego/software/luasocket/)
- [LuaHTTP](https://github.com/daurnimator/lua-http)