---
title:                "Lähettää HTTP-pyyntö"
html_title:           "Haskell: Lähettää HTTP-pyyntö"
simple_title:         "Lähettää HTTP-pyyntö"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko käyttää Haskellia lähettämään HTTP-pyyntöjä? Syy voi olla esimerkiksi tarve hakea tietoja ulkoisesta palvelusta tai lähettää dataa toiselle verkkosivustolle.

## Miten

Haskellissa HTTP-pyynnön lähettäminen on helppoa käyttämällä `http-conduit` -kirjastoa. Ensimmäiseksi asennetaan kirjasto komennolla `cabal install http-conduit`. Sitten voimme käyttää `simpleHttp` -funktiota ja antaa sille URL-osoitteen, jolle haluamme lähettää pyynnön.

```Haskell
import Network.HTTP.Conduit (simpleHttp)

main = do
    response <- simpleHttp "https://www.example.com"
    putStrLn $ "Vastauksen statuskoodi: " ++ show (getResponseStatusCode response)
    putStrLn $ "Vastauksen sisältö: " ++ show (getResponseBody response)
```

Tämä esimerkki lähettää HTTP-pyynnön osoitteeseen `https://www.example.com` ja tulostaa vastauksena saadun statuskoodin ja sisällön. Huomaa, että käytämme `show` -funktiota saadaksemme näytettävän tulosteen `getResponseStatusCode` ja `getResponseBody` -funktioilta.

## Syväsukellus

HTTP-pyynnön lähettämisessä on monia yksityiskohtia, mutta tässä käsitellään vain muutamia tärkeitä asioita. Kun asennat `http-conduit` -kirjaston, saat käyttöösi myös `Request` ja `Response` -tyypit, jotka antavat sinulle tarkemman kontrollin pyynnön lähettämiseen ja vastauksen käsittelyyn.

Esimerkiksi voit asettaa lisäparametreja, kuten otsikkoja ja kehon dataa, `Request` -tyypin avulla. Ja `Response` -tyypistä saat tiedot vastauksen header-osiosta, sisällöstä ja statuskoodista.

Ja jos haluat lähettää pyynnön asynkronisesti tai käyttää eri protokollaa kuin HTTP, voit lukea lisää `http-conduit` -kirjaston dokumentaatiosta ja kurkata muita vaihtoehtoja.

## Katso myös
- [`http-conduit` -kirjaston dokumentaatio](https://hackage.haskell.org/package/http-conduit)
- [Haskelliin liittyvät ohjeet ja neuvoja](https://hackage.haskell.org/package/http-conduit) (englanniksi)