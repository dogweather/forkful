---
title:                "Nedlasting av en nettside"
html_title:           "Haskell: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å laste ned en nettside betyr å hente ned all informasjonen som vises på den til din enhet. Dette er nyttig for programmere som ønsker å samle data fra forskjellige kilder eller for å automatisere visse oppgaver.

## Hvordan:
```Haskell
import Network.HTTP

main = do
  let url = "https://www.example.com"
  getPage url >>= \content -> putStrLn content

getPage url = simpleHTTP (getRequest url) >>= getResponseBody
```
Output:
```
<!doctype html>
<html>
  <head>
    <title>Example Domain</title>
  </head>
  <body>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
  </body>
</html>
```

## Dypdykk:
Å laste ned nettsider har vært en viktig del av internett siden begynnelsen. I dag brukes dette ofte til å samle informasjon til web scraping eller for å hente data til applikasjoner. Alternativene til å bruke Haskell for å laste ned nettsider er å bruke et annet programmeringsspråk som støtter denne funksjonaliteten, eller å bruke et spesialisert verktøy som Wget eller cURL. Implementasjonen av å laste ned en nettside i Haskell er basert på å sende HTTP-forespørsler og behandle svaret.

## Se også:
- [Network.HTTP dokumentasjon](https://hackage.haskell.org/package/HTTP) for mer informasjon om hvordan å bruke dette biblioteket.
- [Haskell for nybegynnere](https://www.haskell.org/koans/) for å komme i gang med Haskell-programmering.
- [Web scraping med Haskell](https://blog.functionally.io/web-scraping-with-haskell/) for å lære mer om hvordan du kan bruke Haskell for å samle informasjon fra nettsider.