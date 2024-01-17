---
title:                "Kahden päivämäärän vertailu."
html_title:           "Elm: Kahden päivämäärän vertailu."
simple_title:         "Kahden päivämäärän vertailu."
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Vertaaminen kahden päivämäärän välillä on tärkeä osa ohjelmoinnissa ja se auttaa meitä käsittelemään aikaa ja päiviä tarkasti. Päivämäärien vertailu mahdollistaa esimerkiksi tapahtumien järjestämisen aikajärjestykseen tai tietyn päivämäärän tarkistamisen.

## Miten:

Tässä on esimerkkejä siitä, kuinka voit vertailla kahta päivämäärää Elm-kielellä:

```Elm
comparison: Date -> Date -> Ordering
comparison date1 date2 =
    if Date.isBefore date1 date2 then
        LT
    else if Date.isAfter date1 date2 then
        GT
    else
        EQ
```

Tämä funktio vertaa kahta päivämäärää ja palauttaa joko "LT" (alle), "GT" (yli) tai "EQ" (yhtä suuri) sen perusteella, mikä päivämäärä on aiempi.

```Elm
birthdayCheck: Date -> Date -> Bool
birthdayCheck date birthdate =
    Date.month date == Date.month birthdate
    && Date.day date == Date.day birthdate
```

Tämä funktio tarkistaa, onko annettuna päivämääränä syntymäpäivä.

## Syväsukellus:

Historiallisessa kontekstissa päivämäärien vertailuun käytettiin paljon käsin koodattuja algoritmeja ennen valmiiksi kirjoitettuja funktioita, kuten Elm-kielessä. Joissakin kielissä, kuten C, päivämäärät voidaan esittää lukuna, ja niitä voidaan sitten vertailla toisiinsa. Yksi vaihtoehto Elm-kielellä on käyttää DateTime-pikakirjoitusta, joka tarjoaa lisätoiminnallisuuksia päivämäärien vertailuun.

## Katso myös:

- [Official Elm Date documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm DateTime package](https://package.elm-lang.org/packages/justinmimbs/elm-date-extra/latest/DateTime)
- [Date comparison in other programming languages](https://www.tutorialspoint.com/how-to-compare-two-dates-in-c-cplusplus)