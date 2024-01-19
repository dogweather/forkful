---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Laste ned WEB-SIDER i Haskell: En Praktisk Guide

## Hva & Hvorfor?

Når vi laster ned en webside, henter vi kildekoden til siden for å bruke den i applikasjonene våre. Dette kan brukes for å skrape data, teste applikasjoner, drive nettanalyse, og mer.

## Hvordan:

Her er en raskt nedlasting av en webside med `http-conduit` pakken.

```Haskell
import Network.HTTP.Conduit (simpleHttp)

main :: IO ()
main = do
  responseBody <- simpleHttp "http://example.com"
  print responseBody
```

Dette vil laste ned HTML-koden fra `http://example.com` og skrive den ut til konsollen.

## Dybdestudie

Å laste ned websider har vært en hoveddel av programvareutviklingsverk siden Webbens begynnelse. I Haskell, det var en gang hvor det meste av denne funksjonaliteten måtte skrives for hånd. Med innføringen av `http-conduit` pakken, er denne prosessen nå enkel og grei.

Alternative metoder kan inkludere bruk av `wget` eller `curl` pakker – disse er robuste og flexible, men kan være overkill for enkel HTML nedlasting.

Når det gjelder implementasjonsdetaljer, sender `simpleHttp` en "GET" forespørsel til den angitte URL-en og returnerer innholdet som en `ByteString`. Denne pakken håndterer også automatisk omadresseringer og kan håndtere HTTPS-forbindelser, noe som gjør den svært nyttig i mange situasjoner.

## Se Også

- [http-conduit på Hackage](http://hackage.haskell.org/package/http-conduit)
- [Haskell's HTTP pakke](http://hackage.haskell.org/package/HTTP)
- [Boken "Real World Haskell"](http://book.realworldhaskell.org/)
- [Haskell's Tutorial for Web Scraping](https://wiki.haskell.org/Web_scraping_with_Haskell)

Husk at programmeringsspråket aldri burde være en begrensing. Bruk det riktige verktøyet for jobben.