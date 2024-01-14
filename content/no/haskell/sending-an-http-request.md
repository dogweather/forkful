---
title:                "Haskell: Sending et HTTP-forespørsel"
simple_title:         "Sending et HTTP-forespørsel"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende en HTTP-forespørsel er en viktig ferdighet for Haskell-programmerere som ønsker å kommunisere med andre nettverkstjenester. Ved å kunne lage og sende en forespørsel, kan vi få tilgang til data fra ulike kilder og bruke dem i våre egne programmer. Dette kan være nyttig for å hente informasjon fra en database, sende data til en API eller laste ned filer fra nettet.

## Hvordan gjør du det

For å sende en HTTP-forespørsel i Haskell, må vi først importere "Network.HTTP.Simple" biblioteket. Deretter kan vi bruke funksjonen "httpRequest" for å lage en forespørsel. Her er et eksempel på å sende en GET-forespørsel til Google:

```Haskell
import Network.HTTP.Simple

-- Lag en GET-forespørsel
request <- parseRequest "GET https://www.google.com"

-- Send forespørselen og få svar
response <- httpLBS request

-- Skriv ut statuskoden og svardata
putStrLn $ "Status kode: " ++ show (getResponseStatusCode response)
putStrLn $ "Svar: " ++ show (getResponseBody response)
```

Dette vil gi oss følgende output:

```
Status kode: 200
Svar: "<!doctype html><html itemscope=\"\" itemtype=\"https://schema.org/WebPage\" lang=\"no\"><head><meta content=\"text/html; charset=UTF-8\" http-equiv=\"Content-Type\"><meta content=\"..."
```

Vi kan også legge til spesifikke parametere i forespørselen, for eksempel å be om å få tilbake JSON-data. Dette gjøres ved å endre URL-en og angi innholdstypen som "application/json":

```Haskell
-- Legg til JSON som innholdstype
request <- parseRequest "GET https://www.example.com/api/users" >>= addRequestHeader "Content-Type" "application/json"
```

## Dykk ned i HTTP-forespørsler

En HTTP-forespørsel består av en forespørselslinje, forespørselsmetode, headers og en eventuell bodies med data. Det finnes flere ulike metoder for å lage og sende HTTP-forespørsler, og mange ulike bibliotek og pakker som kan hjelpe oss med dette. Det er også viktig å være oppmerksom på sikkerhet når vi sender forespørsler, for eksempel ved å bruke TLS-tilkoblinger.

## Se også

- [Haskell Network.HTTP.Simple Documentation](https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Simple.html)
- [HTTP Requests in Haskell - A Practical Guide](https://dandreamsofcoding.com/2016/01/18/http-requests-in-haskell/)
- [Making Simple HTTP Requests in Haskell](https://mmhaskell.com/blog/2017/3/13/making-requests-in-haskell)