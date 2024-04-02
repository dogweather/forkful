---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:00.408647-07:00
description: "Een HTTP-verzoek met basisauthenticatie verzenden betekent dat je programma\
  \ aanklopt bij de deur van een webdienst, waarbij een gebruikersnaam en\u2026"
lastmod: '2024-03-13T22:44:50.851982-06:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek met basisauthenticatie verzenden betekent dat je programma\
  \ aanklopt bij de deur van een webdienst, waarbij een gebruikersnaam en\u2026"
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Wat & Waarom?
Een HTTP-verzoek met basisauthenticatie verzenden betekent dat je programma aanklopt bij de deur van een webdienst, waarbij een gebruikersnaam en wachtwoord worden doorgegeven voor toegang. Programmeurs doen dit om toegang te krijgen tot API's die niet toegankelijk zijn voor het grote publiek of om acties namens een gebruiker uit te voeren.

## Hoe te:
Je hebt het pakket `http-conduit` nodig voor HTTP-acties en `base64-bytestring` voor het coderen van referenties. Importeer ze en gebruik `applyBasicAuth` om referenties aan je verzoek toe te voegen.

```Haskell
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64 (encode)

-- Basisauthenticatieheader construeren
let username = "user"
let password = "pass"
let auth = encode $ pack (username ++ ":" ++ password)

-- Maak je verzoek
request' = parseRequest_ "GET http://example.com/secret"
let request = setRequestHeader "Authorization" ["Basic " <> auth] request'

-- Voer het verzoek uit
response <- httpLBS request

-- Verwerk de respons
print $ getResponseBody response
```

Dit zal de API-respons uitvoeren, als je referenties kloppen.

## Diepere Duik
Basisauthenticatie is oud in webjaren, ontworpen in het begin van de jaren '90, en het is zo simpel als het maar kan: een base64 gecodeerde `gebruikersnaam:wachtwoord` verzonden in een header. Het mist fancy functies zoals tokenverloop en, omdat het onversleuteld is, moet het altijd over HTTPS worden gebruikt.

Alternatieven zoals OAuth bieden veiliger, gedetailleerdere controle. Voor Haskell bieden bibliotheken zoals `http-client` en `wreq` je meer opties en flexibiliteit.

Wat implementatie betreft, vergeet niet om referenties hardcoded in te voeren! Gebruik omgevingsvariabelen of een veilige kluis in productie. En aangezien `base64` codering geen encryptie is (iedereen kan het decoderen), is HTTPS niet alleen een goed idee, maar een must.

## Zie Ook
- Haskell `http-conduit` documentatie: https://hackage.haskell.org/package/http-conduit
- `base64-bytestring` voor codering: https://hackage.haskell.org/package/base64-bytestring
- Voor strakke beveiliging, lees over OAuth2 in Haskell: https://hackage.haskell.org/package/hoauth2
- Lees over de beste praktijken voor het opslaan van geheimen: https://www.yesodweb.com/book/security-considerations
