---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:22.756164-07:00
description: "Een HTTP-verzoek versturen is de handeling van het vragen om data of\
  \ actie aan een webserver. Programmeurs doen dit om te interacteren met API's,\u2026"
lastmod: '2024-03-13T22:44:50.849147-06:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek versturen is de handeling van het vragen om data of actie\
  \ aan een webserver. Programmeurs doen dit om te interacteren met API's,\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?
Een HTTP-verzoek versturen is de handeling van het vragen om data of actie aan een webserver. Programmeurs doen dit om te interacteren met API's, webinhoud te grijpen, of communicatie tussen diensten.

## Hoe:
Laten we naar het leuke gedeelte gaan. Je hebt de `http-client` en `http-client-tls` pakketten nodig. Stel je stack in en voeg ze toe aan je `package.yaml` of `.cabal` bestand. Voer vervolgens `stack build` of de geschikte commando's uit om ze op te halen.

Hier is een simpele GET-aanvraag:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    L8.putStrLn $ responseBody response
```

Dit zal de JSON die je hebt ontvangen van `httpbin.org` afdrukken.

## Diepgaand
Vroeger waren Haskell's HTTP-verzoeken minder rechttoe rechtaan, maar bibliotheken zoals `http-client` hebben het proces vereenvoudigd.

Alternatieven? Zeker. Er zijn `wreq`, `req`, en anderen, vaak met syntactische versieringen of extra functies. Maar `http-client` is als dat betrouwbare Zwitserse zakmes in je lade - het klaart altijd de klus.

Onder de motorkap gebruikt `http-client` een `Manager` om connecties te beheren. Het is efficiënt en hergebruikt sockets. Je kunt het afstemmen, maar standaardinstellingen zijn prima om mee te beginnen.

## Zie Ook
Om je toolkit uit te breiden, bekijk deze:

- [Het `http-client` pakket](https://www.stackage.org/package/http-client)
- [Het `wreq` pakket voor een modernere aanpak](https://www.stackage.org/package/wreq)
- [Hackage voor Haskell-bibliotheken](https://hackage.haskell.org/)
