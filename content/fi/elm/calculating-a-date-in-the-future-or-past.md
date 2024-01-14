---
title:                "Elm: Sekunneista tulevaisuuteen tai menneisyyteen laskenta"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi laskisit aikaa tulevaisuudessa tai menneessä?

Calculating dates in the future or past is a common task in many programming projects. It allows us to plan and keep track of important events, deadlines, or recurring events. In Elm, we can use the built-in `Date` module to easily calculate dates, making it a useful skill for any Elm programmer.

## Miten tehdä

Ajan laskeminen tulevaisuudessa tai menneisyydessä vaatii muutaman vaiheen, jotka on helppo toteuttaa Elm-kielellä. Ensimmäinen vaihe on tuoda käyttöön `Date`-moduuli, joka sisältää tarvittavat työkalut ajan laskemiseen.

```Elm
import Date exposing (Date, toTime, fromTime, add, subtract, millisecond)
```

Seuraavaksi, voimme luoda `Date`-tyypin oliorakenteen valitulla päivämäärällä, tällä kertaa tulevaisuuteen 10 päivää eteenpäin. Tämä voidaan tehdä käyttämällä `add`-funktiota ja antamalla haluttu luku ja aikayksikkö.

```Elm
futureDate : Date
futureDate = add 10 millisecond Date.now
```

Tuloksena saamme päivämäärän 10 päivän päästä tästä hetkestä.

```
2100-01-11T00:00:00.000Z
```

Voimme myös laskea aikaa menneisyydessä käyttämällä `subtract`-funktiota. Esimerkiksi, jos haluamme laskea päivämäärän 5 päivää sitten, käytämme seuraavaa koodia:

```Elm
pastDate : Date
pastDate = subtract 5 millisecond Date.now
```

Tässä tapauksessa saamme päivämäärän 5 päivää taaksepäin tästä hetkestä.

```
2099-12-27T00:00:00.000Z
```

## Syvällinen sukellus

`Date`-moduuli sisältää myös muita hyödyllisiä funktioita, jotka antavat meille enemmän kontrollia päivämäärien laskemisessa. Esimerkiksi `fromTime`-funktio muuttaa millisekunnit `Date`-tyypin olioksi.

```Elm
fromTime : Time -> Date
```

Voimme käyttää tätä yhdessä `toTime`:n kanssa muuttaaksemme `Date`-tyypin takaisin millisekunneiksi.

Toinen hyödyllinen funktio on `difference` joka laskee aikaeron kahden päivämäärän välillä ja palauttaa sen millisekunteina.

```Elm
difference : Date -> Date -> Time
```

Tämä auttaa meitä laskemaan päivien, tuntien ja minuuttien eroja eri päivämäärien välillä.

## Katso myös

- [Elm Date -dokumentaatio](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm Language Guide](https://guide.elm-lang.org/)

Kiitos lukemisesta! Toivottavasti tämä opas auttoi sinua ymmärtämään, kuinka lasketaan aikaa tulevaisuudessa tai menneisyydessä käyttäen Elm-kielellä. Muista tutustua lisäresursseihin jatkaaksesi oppimista. Onnea Elm-ohjelmoinnissa!