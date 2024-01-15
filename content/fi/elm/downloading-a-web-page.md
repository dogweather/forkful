---
title:                "Verkkosivun lataaminen"
html_title:           "Elm: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi:

Olet ehkä kuullut paljon puheita Elm-ohjelmointikielestä ja sen käytöstä web-kehityksessä. Mutta miksi sinun kannattaisi aloittaa käyttämään sitä? Yksi suuri etu Elm:llä on sen kyky hakea web-sivuja ja käsitellä niitä helposti ja turvallisesti. Tässä artikkelissa näytämme, miten se tapahtuu!

## Miten:

Elm tarjoaa valmiita toimintoja web-sivujen lataamiseen ja niiden sisällön käsittelyyn. Seuraavassa esimerkissä käytämme `Http` moduulia lähettämään hakupyyntö ja muuttamaan vastauksen JSON-muotoon:
```Elm
import Http
import Json.Decode exposing (..)

requestUrl : String
requestUrl =
  "https://example.com"

handleResponse : Http.Response -> String
handleResponse response =
  case response of
    Ok body ->
      parseJson body

    Err error ->
      "Oops, something went wrong!"

parseJson : String -> String
parseJson json =
  case decodeValue json of
    Ok result ->
      "JSON-parsed response: " ++ (toString result)

    Err error ->
      "Oops, invalid JSON format!"
```
Kun painat selaimen `nappia`, `requestUrl`-osoitteeseen lähetetään GET-hakupyyntö. Jos vastaus onnistuu, `handleResponse`-funktio ottaa vastaan HTTP-vastauksen `body`:n ja muuttaa sen JSON-muotoon. Lopuksi `parseJson`-funktio tulostaa JSON-parsitun vastauksen tai virheviestin. Tämän avulla voit ladata, käsitellä ja näyttää web-sivun sisällön helposti ja turvallisesti!

## Syvällisemmin:

Elm tarjoaa myös muita moduuleja, kuten `Html`, `Navigation`, ja `Url`, jotka mahdollistavat web-sivujen luomisen ja navigoinnin sivujen välillä. Mutta koska tämä artikkeli keskittyy web-sivujen lataamiseen, suosittelemme tutustumaan `Http` moduulin dokumentaatioon ja kokeilemaan erilaisia koodaamisen mahdollisuuksia!

## Katso myös:

- [Elm:n virallinen dokumentaatio](https://guide.elm-lang.org/)
- [Esimerkkejä Elm-koodista ja projektista](http://elm-lang.org/examples)
- [Lisää tietoa Elm:stä ja sen käytöstä web-kehityksessä](https://medium.com/@_rchaves_/how-to-be-lazy-and-efficient-when-learning-elm-1d3cb3b6b4bb)