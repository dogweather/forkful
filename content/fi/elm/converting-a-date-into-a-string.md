---
title:    "Elm: Muuntaminen päivämääräksi merkkijonoksi"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sinun pitäisi muuntaa päivämäärä merkkijonoksi Elm-ohjelmoinnissa? Usein tarvitsemme esittää päivämäärä käyttäjälle jollakin tavalla, joten päivämäärän muuntaminen merkkijonoksi on hyödyllinen taito jokaiselle ohjelmoijalle.

## Kuinka

Tässä esimerkissä käytämme Elm:n `Date`- kirjastoa päivämäärän muuntamiseen merkkijonoksi. Alla on koodiesimerkki, joka ottaa tänään olevan päivämäärän ja muuntaa sen merkkijonoksi muodossa DD.MM.YYYY.

```Elm
import Date exposing (Day, Month, Year, fromTime, toTime, toParts)
import List exposing (map)

today : Date
today =
    toTime (86400 * Date.now.day)

toString : Date -> String
toString date =
    let
        (Year year) =
            fromTime date

        (Month month) =
            fromTime date

        (Day day) =
            fromTime date

        monthString =
            if month < 10 then
                "0" ++ toString month
            else
                toString month
                
        dayString =
            if day < 10 then
                "0" ++ toString day
            else
                toString day
    in
        dayString ++ "." ++ monthString ++ "." ++ toString year
```

Kun suoritamme tätä funktiota tänään (25.9.2021), saamme tulokseksi "25.09.2021".

## Syväsukellus

Askel askeleelta selitetty koodina, tässä näytämme kuinka käytämme `Date`- kirjastoa muuntamaan päivämäärän merkkijonoksi.

1. Tuomme `Date`- kirjaston käyttöömme sekä `toTime` ja `fromTime` funktiot.
2. Luomme uuden `Date` -muuttujan `today`, joka sisältää tämän päivän päivämäärän.
3. Luomme `toString`- funktion, joka ottaa parametrinaan päivämäärän ja palauttaa merkkijonona muodossa DD.MM.YYYY.
4. Funktiossa otamme käyttöön `toParts`-funktion, joka palauttaa päivämäärän osat vuodeksi, kuukaudeksi ja päiväksi.
5. Muotoilemme kuukauden ja päivän string-muotoon, jotta pystymme lisäämään nollat numeroiden eteen tarvittaessa.
6. Palautamme päivämäärän merkkijonona yhdistämällä päivän, kuukauden ja vuoden string-muodot pisteiden avulla.

## Katso myös

- Elm `Date`-kirjaston [dokumentaatio](https://package.elm-lang.org/packages/elm/time/latest/) 
- Evästeiden muotoilu [Elm`Date`-kirjastolla](https://dev.to/chickenswing/getting-to-the-bottom-of-it-elm-date-string-33m0)