---
title:    "Gleam: Päivämäärän hankkiminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Miksi hankkia nykyinen päivämäärä?

Hankkia nykyinen päivämäärä voi olla tärkeää monista syistä, kuten laskutusta tai aikaleimojen luomista varten. Gleam-ohjelmointikielen avulla voit helposti hankkia tämän tiedon käyttämällä sisäänrakennettua `Date.now()` -funktiota ja tarvittavia kirjastoja.

## Miten tehdä se?

Gleamissa nykyinen päivämäärä voidaan hankkia helposti `Date.now()` -funktion avulla. Tämä palauttaa päivämäärän Unix-aikaleiman muodossa, joka edustaa päivämäärää sekunteina vuoden 1970 1. tammikuuta jälkeen. Voit sitten käyttää Gleamin `Calendar` -kirjastoa muuntaaksesi tämän aikaleiman haluamaasi muotoon, kuten päivänä, kuukautena ja vuotena.

```Gleam
import gleam/calendar

let now = Date.now()

let date = now |> Calendar.from_unix_sec

IO.info(date.day) // 14
IO.info(date.month) // 5
IO.info(date.year) // 2021
```

## Syvempää tarkastelua

`Date.now()` -funktion lisäksi Gleamissa on myös muita tapoja hankkia nykyinen päivämäärä, kuten `Date.utc_now()` ja `Date.local_now()`, jotka palauttavat UTC- ja paikallisen aikavyöhykkeen aikaleimat. Voit myös antaa `Calendar.from_unix_sec` -funktiolle halutun aikavyöhykkeen, jos haluat muuntaa aikaleiman toiseen aikavyöhykkeeseen.

# Katso myös

- [Gleamin dokumentaatio: Date](https://gleam.run/documentation/std-lib-date)
- [Gleamin dokumentaatio: Calendar](https://gleam.run/documentation/std-lib-calendar)