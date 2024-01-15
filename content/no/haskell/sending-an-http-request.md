---
title:                "Sending et http-forespørsel"
html_title:           "Haskell: Sending et http-forespørsel"
simple_title:         "Sending et http-forespørsel"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noensinne har brukt en nettleser eller et program som kobler til Internett, har du sannsynligvis allerede sendt en HTTP forespørsel uten å vite det. HTTP står for Hypertext Transfer Protocol og er den vanligste protokollen som brukes for kommunikasjon mellom nettlesere og webservere. I Haskell, kan du programmere dine egne HTTP forespørsler for å hente data fra et nettsted, sende informasjon til en webtjeneste, eller automatisere en nettopp-kallende oppgave.

## Slik gjør du

For å sende en HTTP forespørsel i Haskell, må du bruke et bibliotek som håndterer HTTP kommunikasjon. Det finnes flere biblioteker tilgjengelig, men et populært valg er "http-conduit". Først må du importere biblioteket i din kode:

```Haskell
import Network.HTTP.Simple
```

Deretter kan du lage en HTTP forespørsel ved å bruke funksjonen "parseRequest" og angi URL-adressen du ønsker å kalle som en streng:

```Haskell
request <- parseRequest "http://www.example.com"
```

Du kan også spesifisere hvilken HTTP metode du vil bruke, som for eksempel GET eller POST:

```Haskell
request <- setRequestMethod "POST" <$> parseRequest "http://www.example.com"
```

Nå kan du sende forespørselen ved å bruke funksjonen "httpLBS". Denne funksjonen returnerer en respons som inneholder både statuskoden og svaret fra nettstedet:

```Haskell
response <- httpLBS request
```

For å få tilgang til svaret fra nettstedet, kan du bruke funksjonen "getResponseBody":

```Haskell
responseBody <- getResponseBody response
```

Du kan nå bruke "responseBody" til å gjøre videre behandling på dataene, for eksempel å pakke de ut fra JSON format, eller skrive de til en fil.

## Dypdykk

Hvis du ønsker å lære mer om hvordan HTTP forespørsler fungerer og alle de forskjellige parameterne du kan spesifisere, kan du sjekke ut den offisielle HTTP spesifikasjonen (https://www.w3.org/Protocols/rfc2616/rfc2616.html) som beskriver alle detaljene. Du kan også utforske "http-conduit" dokumentasjonen for å se alle funksjonene som er tilgjengelige for å bygge mer komplekse forespørsler og håndtere forskjellige situasjoner som feil og omadressering.

## Se også

- Offisiell HTTP spesifikasjon: https://www.w3.org/Protocols/rfc2616/rfc2616.html
- http-conduit dokumentasjon: https://hackage.haskell.org/package/http-conduit/docs/Network-HTTP-Conduit.html