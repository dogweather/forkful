---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
html_title:           "Haskell: HTTP-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen perusautentikoinnin avulla on hyödyllistä, kun haluat varmistaa, että vain tietyn oikeutuksen omaavat käyttäjät pääsevät tiettyihin verkkoresursseihin.

## Kuinka tehdä

Lähettääksesi HTTP-pyynnön perusautentikoinnin avulla, sinun täytyy ensin asettaa Authorization-otsake pyyntöösi. Tämä otsake koostuu käyttäjätunnuksesi ja salasanaasi yhdistävästä Base64-koodatusta merkkijonosta. Voit helposti luoda tämän merkkijonon Haskellilla seuraavalla tavalla:

```Haskell
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (pack)

let username = pack "käyttäjätunnuksesi"
let password = pack "salasanasi"
let credentials = encode $ pack (username ++ ":" ++ password)

```

Nyt voit lisätä tämän credentials-muuttujan Authorization-otsakkeeseen pyynnössäsi ja lähettää sen verkkoresurssille. Esimerkiksi, jos haluat lähettää GET-pyynnön osoitteeseen https://www.example.com/api, jossa käytetään perusautentikointia, voit tehdä sen seuraavasti:

```Haskell
import Network.HTTP.Client (parseUrlThrow, newManager, httpLbs, responseBody, Request(..))
import Network.HTTP.Simple (setRequestMethod, setRequestHeaders)

let url = "https://www.example.com/api"
let request = setRequestHeaders [("Authorization", pack ("Basic " ++ credentials))]
              $ setRequestMethod "GET"
              $ fromJust $ parseUrlThrow url

manager <- newManager defaultManagerSettings
response <- httpLbs request manager
putStrLn $ responseBody response
```

Tämän esimerkkikoodin avulla saat ladattua verkkoresurssin sisällön ja tulostettua sen konsolille. Muista vaihtaa url-muuttujan arvio oikeaksi pyyntöäsi vastaavaksi.

## Syventyminen

On tärkeää huomata, että Base64-koodattu käyttäjätunnuksesi ja salasanasi ovat välitön osa pyyntösi otsaketta. Tämä tarkoittaa, että näitä tietoja ei tulisi lähettää avoimesti ilman suojaa, sillä ne voivat helposti paljastaa pääsyn tärkeisiin verkkoresursseihin. On suositeltavaa käyttää HTTPS-yhteyttä lähettäessäsi perusautentikoituja pyyntöjä, sillä se kryptaa kaiken tiedon välillänne.

## Katso myös

- [HTTP-ohjelmointimoduulit Hackage-repositoriossa](https://hackage.haskell.org/packages/search?terms=HTTP)
- [RFC 2617 - HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)