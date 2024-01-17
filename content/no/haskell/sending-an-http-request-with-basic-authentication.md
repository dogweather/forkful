---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Haskell: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering er en måte for programmerere å sikre at bare autoriserte brukere kan få tilgang til en bestemt ressurs eller data. Dette kan være nyttig i situasjoner der sensitiv informasjon skal beskyttes eller når det kreves en form for brukeridentifikasjon for å få tilgang til ressurser.

## Hvordan:
Haskell tilbyr enkelt å sende en HTTP-forespørsel med grunnleggende autentisering ved hjelp av ```http-client``` biblioteket. Her er et eksempel på hvordan du kan gjøre det:

```haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "https://example.com"
    let request' = applyBasicAuth "username" "password" request
    response <- httpLbs request' manager
    putStrLn $ show response
```

Dette eksemplet vil sende en GET-forespørsel til nettstedet "https://example.com" med brukernavn og passord som brukes til å autentisere forespørselen. Den resulterende responsen vil bli skrevet ut til konsollen.

## Dypdykk:
Grunnleggende autentisering har vært en del av HTTP-spesifikasjonene siden 1990-tallet og er en av de mest brukte autentiseringsmetodene. Alternativene til grunnleggende autentisering inkluderer Digest-autentisering og OAuth. Når du implementerer grunnleggende autentisering, er det viktig å sørge for at forbindelsen er kryptert for å hindre uautorisert tilgang og potensiell eksponering av brukernavn og passord.

## Se også:
For mer informasjon om å sende HTTP-forespørsler i Haskell, kan du sjekke ut dokumentasjonen til ```http-client``` biblioteket og lære mer om autentiseringstyper og implikasjoner på webutvikling over hele nettet.