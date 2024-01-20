---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel er en prosess der et program sender en forespørsel til en server for å hente eller oppdatere data. Programmerere gjør dette for å få tilgang til en tjeneste som en webserver tilbyr, slik som å hente nettsideinformasjon, eller manipulere data i en database.

## Hvordan:

Først, installere 'socket.http' bibilioteket i Lua:

```Lua
-- importer socket.http biblioteket
http = require("socket.http")

-- send en GET forespørsel og skriv ut respons
http.request{
  url = "http://www.google.com", 
  sink = ltn12.sink.file(io.stdout)
}
```

I dette eksemplet har vi sendt en GET forespørsel til google.com, og skrev ut responsen.

## Dykking

HTTP-forespørsel har sin opprinnelse i HTTP-protokollen, som ble opprettet i 1989 av Tim Berners-Lee på CERN. Det er benyttet for det meste til web visningsoperasjoner.

Alternativer til 'socket.http' inkluderer blant annet 'luasocket' og 'lua-http' bibliotekene. Valget mellom disse vil avhenge av dine spesifikke krav, som ytelse, letthet av bruk, og støtten for forskjellige HTTP-metoder.

En HTTP-forespørsel kan bli sendt ved å bruke forskjellige metoder som GET, POST, DELETE, osv., avhengig av hvilken operasjon du vil utføre på serveren. Disse forespørslene kan inneholde hodedata, slik som innholdstype og lengde, og et meldingslegeme, som bildedata eller formdata.

## Se Også 

1. [HTTP-forespørselsmetoder](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
2. [Lua 5.1 Nettverksstøtte: socket.http](http://w3.impa.br/~diego/software/luasocket/http.html)
3. [Lua HTTP-forespørselsbiblioteker](https://luarocks.org/modules/luarocks/luajit-request)

Ikke glem å prøve ut forskjellige forespørselsmetoder og se hvordan serveren reagerer. Lykke til med læringen!