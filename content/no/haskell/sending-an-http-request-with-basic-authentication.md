---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering handler om å opprette en klarert kommunikasjon mellom klient og server. Programmerere gjør dette for å bygge sikre systemer, som krever brukere å bekrefte sin identitet først.

## Hvordan:

For å sende en HTTP-forespørsel med grunnleggende autentisering i Haskell, kan du bruke `http-client`-biblioteket. Sjekk ut dette enkle eksemplet.

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

main = do
    manager <- newManager tlsManagerSettings
    let request = applyBasicAuth "username" "password" $ parseRequest_ "http://example.com"
    response <- httpLbs request manager
    print $ responseBody response
```
Hvis alt går etter planen, skal du se responsen fra serveren i output.

## Dypdykk

1. *Historisk kontekst*: Grunnleggende autentisering er en av de eldste metoder for identitetskontroll på weben. Det har vært med oss siden vedlegg til HTTP/1.0 spesifikasjonen på midten av 1990-tallet.

2. *Alternativer*: Selv om grunnleggende autentisering er ganske enkel å implementere, er det ikke alltid den beste løsningen. For eksempel, OAuth 2.0, token-basert autentisering, og session-basert autentisering er noen alternative metoder som tilbyr bedre sikkerhet for særlig sensitive applikasjoner.

3. *Implementeringsdetaljer*: Grunnen til at vi bruker `applyBasicAuth` funksjonen i eksemplet ovenfor er å legge til en "Authorization"-header i HTTP-forespørselen. Denne headeren inneholder det brukernavn og passord vi oppga, kryptert med Base64.

## Se også

- [`http-client` dokumentasjon](https://hackage.haskell.org/package/http-client)
- [HTTP/1.0 spesifikasjonen](https://tools.ietf.org/html/rfc1945)
- [OAuth 2.0](https://tools.ietf.org/html/rfc6749)