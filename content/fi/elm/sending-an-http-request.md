---
title:                "Lähettäminen http-pyyntö"
html_title:           "Elm: Lähettäminen http-pyyntö"
simple_title:         "Lähettäminen http-pyyntö"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

HTTP-pyynnön lähettäminen tarkoittaa tiedon lähettämistä internetin kautta. Tämä on tärkeä osa ohjelmointia, sillä se mahdollistaa tietojen hakemisen ja vastaanottamisen eri palvelimilta verkossa.

## Kuinka tehdä:

```Elm
import Http

Http.send Http.getString "https://example.com/"
    |> Task.perform (Result.toString "Error getting data") (\response -> 
        -- Tee jotain vastaukselle
        )
```

Esimerkissä käytetään Elm:n `Http`-kirjastoa, joka tarjoaa kätevän tavan lähettää HTTP-pyyntöjä. Pyyntö tehdään `send`-funktiolla ja käytetään `Task.perform`-funktiota käsittelemään vastaus. Vastauksen voi käsitellä haluamallaan tavalla, kuten tallentaa se muuttujaan tai tulostaa se konsolille.

## Syvällistä tietoa:

HTTP eli Hypertext Transfer Protocol kehitettiin jo 1990-luvulla ja siitä on tullut standardi tiedonsiirrossa internetin kautta. On myös olemassa muita tapoja lähettää ja vastaanottaa tietoa verkossa, kuten WebSockets ja GraphQL, mutta HTTP on yhä yleisimmin käytetty protokolla.

Elm tarjoaa myös muita tapoja lähettää HTTP-pyyntöjä, kuten `Http.post` ja `Http.request`, jotka mahdollistavat tarkemman määrittelyn esimerkiksi headereille ja lomaketiedoille.

## Katso myös:

- Elm `Http`-kirjaston dokumentaatio: https://package.elm-lang.org/packages/elm/http/latest/
- HTTP-protokollan historia: https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#History