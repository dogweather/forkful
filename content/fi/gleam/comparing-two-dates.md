---
title:                "Gleam: Kahden päivämäärän vertailu"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi
Jokainen ohjelmoija tarvitsee joskus verrata kahta päivämäärää toisiinsa, olipa kyseessä sitten aikataulujen tarkastelu tai tapahtumien järjestäminen. Gleamin avulla tämä on helppoa ja tehokasta.

## Miten tehdä
```Gleam
import Gleam.Date

let date1 = Date.new(2021,7,15)
let date2 = Date.new(2021,7,1)

let date1_is_after_date2 = Date.is_after(date1, date2) // true
```

Tässä esimerkissä luomme kaksi uutta päivämäärää Gleam Date -moduulin avulla ja käytämme sitten `is_after` -funktiota verrataksemme, onko ensimmäinen päivämäärä myöhempi kuin toinen. Gleamin avulla voit myös helposti tarkastella muita päivämäärään liittyviä tietoja, kuten vuoden, kuukauden ja päivän eri osia.

## Syväsukellus
Gleam tarjoaa kattavan Date-moduulin, joka sisältää useita hyödyllisiä funktioita ja tietotyyppejä päivämäärän käsittelyyn. Voit esimerkiksi muuntaa päivämäärän eri muotoihin, kuten merkkijonoksi tai Unix-aikaleimaksi, tai tarkistaa päivämäärän oikeellisuuden.

## Katso myös
- [Gleamin virallinen dokumentaatio Date-moduulista](https://gleam.run/articles/dates)
- [Esimerkkejä Gleamin Date-moduulin käytöstä](https://github.com/gleam-lang/gleam/blob/master/lib/date/examples/main.gleam)