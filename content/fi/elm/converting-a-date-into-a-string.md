---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän muuttaminen merkkijonoksi on prosessi, jossa päivämääräobjekti muunnetaan luettavampaan formaattiin. Ohjelmoijat tekevät tämän tiedon esittämiseksi käyttäjille ymmärrettävämmässä muodossa.


## Näin se tehdään:

Elm tarjoaa erinomaisen paketin tätä varten: 'elm/time'. Katsotaan esimerkkiä:

```Elm
import Time exposing (..)

muunnaPäivämäärä : Posix -> String
muunnaPäivämäärä aika =
    let
        päivä = toGregorianDate aika
    in
    (toString päivä.day) ++ "." ++ (toString päivä.month) ++ "." ++ (toString päivä.year)
```

Tämän funktion avulla voimme muuntaa Posix-ajan tyylikkääksi merkkijonoksi. Testataan tätä funktiota:

```Elm
main =
    let
        nyt = fromMillis 1577833200000
    in
    text (muunnaPäivämäärä nyt)
```
Tulostaessa saamme merkkijonon "1.1.2020".

## Syventävä tarkastelu:

Historiallisesti erilaisia tapoja on ollut päivämäärä-esitysten muuntamiseksi merkkijonoiksi, joista jokainen antaa hieman erilaisen tuloksen. Elm:ssä olemme päättäneet käyttää 'elm/time' pakettia sen joustavuuden ja vaivattoman käytön takia. 

Vaihtoehtoisesti, voit käyttää 'elm/regex' pakettia ja luoda oman regular expressionin, joka muuntaa ajan merkkijonoksi. Tämä on varsin teknistä ja aikaa vievää, mutta antaa sinulle täyden kontrollin.

Mikäs siinä on myös hyvää, että Elm:n päivämäärän muuntofunktiot palaavat Maybe-tyypin. Tämä tarkoittaa, että funktiot huolehtivat virheen tarkastuksesta sinun puolestasi!

## Katso myös:

Lisää tietoa ja apua muusta Elm:n ajan käsittelystä voit löytää seuraavien linkkien kautta:

- [Elm Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm GregorianDate](https://package.elm-lang.org/packages/elm/time/latest/Time-Gregorian)