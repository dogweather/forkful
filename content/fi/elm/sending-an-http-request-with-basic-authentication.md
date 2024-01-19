---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lähettäminen HTTP-pyynnöllä perusautentikoinnilla tarkoittaa verkkopyynnön lähettämistä verkolle tunnistustietojen, kuten käyttäjänimen ja salasanan, avulla. Ohjelmoijat tekevät tämän tarkistaakseen käyttäjän oikeudet ennen kuin heille myönnetään pääsy verkkosivuston tai järjestelmän resursseihin.

## Kuinka:
```Elm
import Http
import Http.Headers as Headers

...

lähetäPyynnöllä : String -> String -> Http.Request String
lähetäPyynnöllä käyttäjänimi salasana =
    let
        auth =
            "Basic " ++ (käyttäjänimi ++ ":" ++ salasana |> Http.harEncode)
    in
    Http.request
        { method = "GET"
        , headers = [ Headers.authorization auth ]
        , url = "https://example.com/authenticated-endpoint"
        , body = Http.emptyBody
        , expect = Http.expectString GotResponse
        , timeout = Nothing
        , tracker = Nothing
        }

...
```

## Syvällisempi tarkastelu
HTTP-pyynnöllä perusautentikointi on historiallisesti yksi vanhimmista verkkotunnistusmenetelmistä, joka on kehitetty aikana, jolloin turvallisuus ei ollut suuri huolenaihe. Nykypäiväisissä sovelluksissa on muiden vaihtoehtojen, kuten OAuth tai JWT, joiden avulla ohjelmoijat voivat tarjota parempaa turvallisuutta käyttäjätunnistuksen yhteydessä.

Elm:ssä HTTP-pyyntö on mahdollista lähettää perustietojen käsittelyllä käyttämällä "authorization" header, johon lisätään käyttäjätunnus ja salasana Base64-koodattuna merkkijonona. 

Muista olla käyttämättä HTTP-perusautentikointia yli avoimen, salaamattoman verkon - tämä lähettäisi salasanasi selkokielisenä lankaansa pitkin!

## Katso myös:
- [Elm:n Http kirjasto](https://package.elm-lang.org/packages/elm/http/latest/)
- [Elm:n Http.Headers kirjasto](https://package.elm-lang.org/packages/elm/http/latest/Http-Headers) 
- [RFC 7617, Basic Authentication Scheme](https://tools.ietf.org/html/rfc7617)