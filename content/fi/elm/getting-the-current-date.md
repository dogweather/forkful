---
title:                "Elm: Nykyisen päivämäärän hakeminen."
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Elm on suosittu ja suhteellisen uusi ohjelmointikieli, joka on suunniteltu helpottamaan verkkosovellusten kehittämistä. Yksi sen hyödyllisimpiä ominaisuuksia on sen kyky saada nykyinen päivämäärä ja aika, mikä on tärkeä toiminto monille sovelluksille. Tässä blogikirjoituksessa kerromme, miten voit käyttää Elm-kieltä saadaksesi nykyisen päivämäärän ja ajan.

## Miten

Getting the current date with Elm is a simple task. First, you will need to import the `Time` module into your Elm program. Then, you can use the built-in function `now` to get the current date and time. Here is an example code block:

``` Elm
import Time exposing (now)

currentDate = now
```

The output will be a `Time.Posix` value, which represents the current date and time in milliseconds. This value can then be converted into a readable format using the `Time` module functions, such as `Time.toDate` or `Time.toString`.

## Syväsukellus

Saadaksesi tarkemman käsityksen siitä, miten `now`-funktio toimii, voit tutustua Elm-kielen dokumentaatioon ja lähdekoodiin. On myös hyödyllistä tutkia `Time` moduulin muita toimintoja, kuten `fromCalendar` ja `atMidnight`, jotka tarjoavat erilaisia tapoja käsitellä päivämääriä ja aikoja Elm-ohjelmassa.

Lisäksi on tärkeää huomata, että saadessaan nykyisen päivämäärän ja ajan `now`-funktio käyttää tietokoneen paikallista aikavyöhykettä. Tämä on hyödyllistä ottaa huomioon niille, jotka käyttävät sovellustasi eri aikavyöhykkeillä.

## Katso myös

- [Elm-ohjelman aloittaminen] (https://guide.elm-lang.org/)
- [Time Moduulin dokumentaatio] (https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Elm-kielen lähdekoodi] (https://github.com/elm/compiler)

Kiitos kun luit tämän blogikirjoituksen saadaksesi lisätietoja siitä, miten saat nykyisen päivämäärän ja ajan Elm-ohjelmalla. Toivottavasti tämä auttoi sinua ymmärtämään tätä toimintoa paremmin ja voit nyt käyttää sitä tehokkaasti sovelluksissasi. Onnea koodaukseen!