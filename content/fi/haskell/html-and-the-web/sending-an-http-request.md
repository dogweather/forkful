---
date: 2024-01-20 17:59:56.180525-07:00
description: "How to: - Kuinka: Haskellissa HTTP-pyynt\xF6jen l\xE4hett\xE4miseen\
  \ k\xE4ytet\xE4\xE4n kirjastoja, kuten `http-conduit`. Esimerkiksi n\xE4in."
lastmod: '2024-04-05T21:53:58.181328-06:00'
model: gpt-4-1106-preview
summary: ''
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

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
