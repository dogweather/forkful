---
title:                "Haskell: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyynnön lähettäminen on olennainen osa monien sovellusten toimintaa. Se mahdollistaa tietojen lähettämisen palvelimelle ja vastauksen vastaanottamisen. Tämä on tärkeä prosessi esimerkiksi verkkosovelluksissa ja API-yhteyksissä.

## Kuinka tehdä

HTTP-pyynnöt voidaan lähettää Haskellilla helposti käyttämällä `Network.HTTP` -kirjastoa. Alla on esimerkki GET-pyynnön lähettämisestä suomennettuun verkkosivustoon.

```Haskell
-- Tuodaan kirjasto
import Network.HTTP

-- Määritetään URL-osoite ja tehdään pyyntö
url = "http://www.example.com"
request = getRequest url

-- Suoritetaan pyyntö ja tallennetaan vastaus
response <- simpleHTTP request
body = responseBody response

-- Tulostetaan vastauksen sisältö
putStrLn body
```

Tämänkaltainen koodi tulostaisi verkkosivuston HTML-koodin konsoliin.

## Syväsukellus

HTTP-pyynnön lähettämiseen liittyy monia muita ominaisuuksia, kuten pyyntöjen muokkaaminen, virheiden käsittely ja salattujen yhteyksien luominen. On tärkeää tutustua tarkemmin `Network.HTTP` -kirjaston dokumentaatioon, jotta pystyt lähettämään pyyntöjä haluamallasi tavalla.

## Katso myös

- [HTTP Requestin lähetys Nettiyhteys Haskell kirjastolla] (https://stackoverflow.com/q/16058006/13130406)
- [Kirjaston Network.HTTP dokumentaatio] (https://hackage.haskell.org/package/HTTP)
- [HTTP-pyyntöjen verkkokirjasto Haskellilla] (https://mmhaskell.com/blog/2017/5/15/working-with-web-apis-in-haskell)