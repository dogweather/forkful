---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Last ned en nettside med Lua

## Hva & Hvorfor?

Å laste ned en webside betyr å hente data fra en webserver til din lokale maskin. Programmerere gjør dette for å få tilgang til webinnhold for parsing, scrapping, eller offline bruk.

## Slik gjør du:

Her er et enkelt eksempel på hvordan du kan laste ned en webside med Lua ved bruk av luasocket biblioteket.

```Lua
http = require("socket.http")
url = "http://www.google.com"

function download_page(url)
  local page = http.request(url)
  return page
end

local result = download_page(url)
print(result)
```

Når du kjører koden over, bør du se HTML-innholdet i Google hjemmesiden som utskrift i konsollen.

## Dypdykk

- **Historisk kontekst:** Lua ble designet i 1993 av Roberto Ierusalimschy, Luiz Henrique de Figueiredo, og Waldemar Celes fra Pontifical Catholic University of Rio de Janeiro i Brasil. Lua er superlett og fleksibelt, og er dermed en favoritt for innbygd bruk, prototyping og scripting.
- **Alternativer:** Du kan laste ned websider ved hjelp av mange programmeringsspråk, nylig er Python og dens 'requests' bibliotek også mye brukt. Men valget av språk avhenger av mange faktorer; for eksempel bruker du Lua hvis du skal jobbe med innebygde systemer eller spillutvikling.
- **Implementering detaljer:** Lua bruker en modul kalt socket.http for å håndtere HTTP-kommunikasjon. Dette er innpakket av `http.request` funksjonen, som sender en HTTP GET forespørsel til URLen som ble angitt.

## Se også

- Lua 5.4 reference manual: https://www.lua.org/manual/5.4/
- LuaSocket manual: http://w3.impa.br/~diego/software/luasocket/http.html
- Artikkel om hvordan å bruke Lua for Web Scraping: https://www.freecodecamp.org/news/scraping-in-lua/