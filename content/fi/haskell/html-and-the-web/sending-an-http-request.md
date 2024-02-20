---
date: 2024-01-20 17:59:56.180525-07:00
description: "L\xE4hett\xE4m\xE4\xE4n HTTP-pyynn\xF6n tarkoittaa, ett\xE4 pyydet\xE4\
  \xE4n tietoja verkossa olevasta palvelimesta tai l\xE4hetet\xE4\xE4n dataa sille.\
  \ Ohjelmoijat tekev\xE4t t\xE4m\xE4n, koska se\u2026"
lastmod: 2024-02-19 22:05:15.505645
model: gpt-4-1106-preview
summary: "L\xE4hett\xE4m\xE4\xE4n HTTP-pyynn\xF6n tarkoittaa, ett\xE4 pyydet\xE4\xE4\
  n tietoja verkossa olevasta palvelimesta tai l\xE4hetet\xE4\xE4n dataa sille. Ohjelmoijat\
  \ tekev\xE4t t\xE4m\xE4n, koska se\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Lähettämään HTTP-pyynnön tarkoittaa, että pyydetään tietoja verkossa olevasta palvelimesta tai lähetetään dataa sille. Ohjelmoijat tekevät tämän, koska se on tapa hakea, lähettää ja vaihtaa tietoa eri järjestelmien välillä.

## How to: - Kuinka:
Haskellissa HTTP-pyyntöjen lähettämiseen käytetään kirjastoja, kuten `http-conduit`. Esimerkiksi näin:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple

main :: IO ()
main = do
  response <- httpLBS "http://httpbin.org/get"
  putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
  putStrLn $ "The response body was: " ++ show (getResponseBody response)
```

Aja ja odota tuloste:

```
The status code was: 200
The response body was: "{\"args\":{},\"headers\":{...},\"origin\":\"...\",\"url\":\"http://httpbin.org/get\"}"
```

## Deep Dive - Syväsukellus:
HTTP-pyynnöt ovat olleet osa webin rakennetta alusta lähtien, 1990-luvun alusta. Vaihtoehtoja `http-conduit`:lle ovat esimerkiksi `wreq` ja `req`, jotka tarjoavat eri tasoja abstraktiosta. `http-conduit`:ssa pyynnöt rakentuvat `ByteString`-tyypin varaan, joka on tehokas binääridatan käsittelyssä.

Sisäisesti, Haskellin HTTP-kirjastot käyttävät monadiittista IO:tä operoidakseen side-effektien kanssa, kuten verkko-operaatiot. Joustavuus tulee korkean tason abstraktioista, kuten functioista, jotka kääntelevät HTTP-pyyntöjä ja -vastauksia, samaan aikaan kun kielen laiskuus mahdollistaa tehokkaan datan käsittelyn.

## See Also - Katso Myös:
Täydentäviä resursseja ja dokumentaatioita:

- http-conduit kirjaston ohjeet: https://www.stackage.org/package/http-conduit
- Wreq-kirjasto: http://www.serpentine.com/wreq/
- Req-kirjasto: https://hackage.haskell.org/package/req
- Real World Haskell -kirjan luku HTTP:stä: http://book.realworldhaskell.org/read/programming-with-monads.html
