---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Hankkiminen nykyinen päivämäärä tarkoittaa päästä tietää, minkä päivämäärän tänään on. Ohjelmoijat tekevät tämän, jotta sovellus voi toimia ajantasaisesti tai viitata siihen, milloin jokin tapahtuma tapahtui.

## Kuinka tehdä:
Alla on esimerkki siitä, miten saat nykyisen päivämäärän Elm-ohjelmointikielellä.

```Elm
import Time exposing (Posix, Zone, utc, posixToTime)
import Task

nykyinenPäivämäärä : Task.Task x Posix
nykyinenPäivämäärä =
    Time.now
```
Kun suoritat tämän koodin, tuotos on Posix-timestamp, joka on nykyinen aika. Voit muuntaa sen ihmisen luettavaksi.

```Elm
import Time exposing (Posix, toHour, toMinute, utc, posixToTime)
import Task

timestampToString : Posix -> String
timestampToString posix =
    let
        hour =
            toHour utc posix

        minute =
            toMinute utc posix
    in
    (String.fromInt hour) ++ ":" ++ (String.fromInt minute)

getHumanReadableDate : Task.Task x String
getHumanReadableDate =
    Time.now
        |> Task.andThen (Task.succeed << timestampToString)
```

## Syvempi Sukellus
Historiallisesti ottaen, Elm-version 0.19 esitteli uuden tavan työskennellä päivämäärä- ja aikatietojen kanssa, joka korvasi vanhemman `Time`-moduulin. Tämä uusi lähestymistapa on ottanut käyttöön `Posix`-tyypin, jonka avulla voimme käsitellä ajanhetkiä, kuten 'nykyinen päivämäärä'.

Tapoja on kaksi:

- `Time.now` on epäsynkroninen prosessi, joka palauttaa nykyisen ajan `Posix`-muodossa.
- `Time.here` palauttaa paikallisen aikavyöhykkeen, minkä avulla voit muuntaa aika-arvoja käyttäjän nykyisen aikavyöhykkeen mukaisesti.

Tämänhetkisen päivämäärän hankkimisen toteutus perustuu suurelta osin JavaScriptin `Date`-objektiin. Elm suorittaa JavaScript-koodin haastaakseen laitteen nykyisen päivämäärän ja kellonajan.

## Katso Myös
Lisätietoja Elm-kielestä ja ajan käsittelystä Elm:ssä muihin lähteisiin:

- Elm:n virallinen dokumentaatio: https://elm-lang.org/docs
- Elm Time-paketti: https://package.elm-lang.org/packages/elm/time/latest/
- Elm-kielen perusteet: https://elmprogramming.com/