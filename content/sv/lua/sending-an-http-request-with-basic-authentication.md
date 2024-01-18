---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Lua: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Vad & Varför?
När man skickar en HTTP-förfrågan med grundläggande autentisering skickar man en begäran till en webbplats med en nyckel för att verifiera sin identitet. Programmerare gör detta för att kunna utföra åtgärder på webbplatser som kräver en användaridentitet för åtkomst, t.ex. att hämta data från en API.

## Hur går man tillväga?
Här är ett enkelt exempel på hur man skickar en HTTP-förfrågan med grundläggande autentisering i Lua:

```Lua
local http = require "socket.http"
local ltn12 = require "ltn12"

-- Skapa en autentiseringssträng baserad på användarnamn och lösenord
local credentials = "användarnamn:lösenord"

-- Skapa en tabell med nödvändig information för HTTP-förfrågan
local request = {
    method = "GET",
    url = "https://exempel.com/api/data",
    headers = {
        ["Authorization"] = "Basic " .. mime.b64(credentials) -- konvertera till Base64
    },
    sink = ltn12.sink.table(respBody) -- spara svar i en tabell
}

-- Utför förfrågan
local response, status_code, response_headers, status_line = http.request(request)

-- Skriv ut svarsbodyn
print(response)
```

Exempel-utdata:

```Lua
Hello, world! This is an authorized response.
```

## Djupdykning
HTTP-förfrågan med grundläggande autentisering skapades först i HTTP/1.0-standarden och används fortfarande i stor utsträckning för att säkra åtkomst till webbplatser och API:er. Alternativ till grundläggande autentisering inkluderar OAuth och mer avancerade autentiseringsmetoder som används för komplexa behörighetssystem. Implementationen av HTTP-förfrågan med grundläggande autentisering i Lua beror på vilken HTTP-bibliotek som används, men den grundläggande principen är densamma.

## Se även
- [HTTP Authentication: Basic and Digest Access Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) from MDN Web Docs
- [HTTP-basic-lua](https://github.com/moteus/lua-http-basic) – ett HTTP-bibliotek för Lua som stöder grundläggande autentisering