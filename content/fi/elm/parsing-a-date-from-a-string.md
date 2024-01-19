---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämääräjonojen jäsentäminen tarkoittaa sitä, että otetaan merkkijono, joka edustaa tietyn päivämäärän, ja muunnetaan se päivämääräolioksi. Tämä on hyödyllistä, koska se tekee päivämäärädatasta helposti käsiteltävää ja vertailtavaa.

## Näin se tehdään:

```elm
import Time

parsiDate : String -> Maybe Time.Posix
parsiDate input =
    Time.fromIsoString ("2020-12-01" ++ "T12:00:00Z")
```

Yllä oleva koodiesimerkki ottaa sisään stringin muodossa olevan päivämäärän ja yrittää muuntaa sen järjestelmänymmärtämään muotoon. Ellei muunnos onnistu, palautetaan `Nothing`.

## Syvempi sukellus

Historiallisesti päivämäärän jäsentämistä merkkijonosta on tarvittu ohjelmoinnissa melko yleisesti, koska tiedonvaihto tietojärjestelmien välillä hoidetaan usein merkkijonojen avulla. Elm tarjoaa oletusarvoisesti ISO 8601 -muodon tukemisen kautta tarvittavat työkalut.

Elmiä käytettäessä on myös mahdollista hyödyntää kolmannen osapuolen kirjastoja, jotka tarjoavat laajemman tuen eri päivämäärämuodoille. Esimerkiksi `rtfeldman/elm-iso8601-date-strings` on hyvä vaihtoehto harkita.

Jäsennys tapahtuu aikavyöhykkeestä riippumatta, mikä tarkoittaa, että sisäisesti päivämäärätiedot tallennetaan sekunteina UNIX-ajan alusta lähtien. Tämän takia on tärkeää olla tietoinen mahdollisista aikavyöhykkeen aiheuttamista eroista, jos päivämääriä käsitellään kansainvälisesti.

## Katso myös 

- [Elm Time dokumentaatio](https://package.elm-lang.org/packages/elm/time/latest/)
- [rtfeldman/elm-iso8601-date-strings](https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/)
- [Opettavainen artikkeli päivämäärien käsittelystä Elm:ssä](https://korban.net/posts/elm/2019-12-08-handling-dates-times-elm/)