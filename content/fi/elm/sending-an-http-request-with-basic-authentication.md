---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
html_title:           "Elm: HTTP-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Lähettäessäsi HTTP-pyynnön perustason todennuksella, varmistat että pyyntöön liitetty data on turvallista ja vain oikeutettujen käyttäjien saatavilla. Tämä on tärkeää, koska ilman todennusta kuka tahansa voisi saada pääsyn arkaluontoiseen tietoon ja aiheuttaa vahinkoa.

## Kuinka?

Esimerkiksi, jos haluat lähettää GET-pyynnön, jossa on perustason todennus, voit tehdä sen seuraavalla tavalla Elm-koodilla:

```Elm
import Http
import Basics exposing (..)
import Bytes exposing (Bytes)

httpRequest : Http.Request
httpRequest =
  { method = "GET"
  , headers = [ Http.basicAuth "käyttäjänimi" "salasana" ]
  , url = "https://www.example.com"
  , body = Http.emptyBody
  , expect = Http.expectBytes (\_ -> true)
  }
```
Tämä luo HTTP-pyynnön, jossa on otsikkona käyttäjänimi ja salasana perustason todennusta varten. Voit myös vaihtaa *GET*-komennon *POST*:iin, jos haluat lähettää tiedon pyyntöön.

## Syvemmälle

Historiallisesti perustason todennus on ollut yleinen tapa suojata HTTP-pyyntöjä. Nykyään se on kuitenkin korvattu monilla muilla tavoilla, kuten token-todennuksella ja OAuth:lla. Ne tarjoavat lisätoiminnallisuuksia ja enemmän tietoturvaa verrattuna perustason todennukseen.

Elm tarjoaa myös muita biblioteekkeja, kuten *elm-http-builder*, jotka voivat auttaa sinua luomaan HTTP-pyynnön perustason todennuksella eri tavalla. Voit myös tutustua tarkemmin HTTP-protokollan toimintaan ja miten perustason todennus siihen liittyy.

## Katso myös

Mahdollisuuksien mukaan suosittelemme aina käyttämään suositeltuja tietoturvakäytäntöjä, kuten token-todennusta. Tutustu myös ELMin virallisiin dokumentaatioihin ja resursseihin, kuten *elm-http*-kirjastoon, oppiaksesi lisää HTTP-pyynnön lähettämisestä ja tietoturvasta.