---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta en webbsida innebär att man skickar en förfrågan till en server för att få tillbaka HTML, CSS och Javascript för den sidan. Programmerare gör detta för att samla data, testa tjänster, övervaka webbplatser, osv.

## Så gör du:
```Lua
-- Installera Socket och HTTP-bibliotek
http = require("socket.http")

-- URL:en till den webbsida du vill hämta
local url = "http://www.example.com"

-- Hämta webbsidan
local response, status, headers = http.request(url)

-- Skriv ut hämtad HTML-kod
print(response)
```
Upprätta en anslutning med "socket.http"-biblioteket, skicka en förfrågan till en bestämd URL och skriv ut serverns svar.

## Djupdykning
Historiskt sett började nedladdning av webbsidor som en enkel GET-förfrågan över HTTP. Idag finns det många metoder att ladda ner sidor, bland annat via verktyg som curl och wget, eller med programmeringsspråk som Python, Ruby och nu Lua.

Alternativ kan variera. Du kan skrapa en webbsida, använda ett API, eller utföra en mängd olika kriterier för att komma åt data.

Lua implementeringen är ganska grundläggande. Den använder "socket.http" biblioteket för att skicka en GET-förfrågan och hantera svaret. Koden kan förbättras genom att lägga till felhantering, hålla HTTP-huvuden och eventuellt tolka och bearbeta den mottagna HTML.

## Se Även
- [Lua socket biblioteket](http://w3.impa.br/~diego/software/luasocket/http.html)
- [HTTP/1.1 specifikation](https://tools.ietf.org/html/rfc2616)