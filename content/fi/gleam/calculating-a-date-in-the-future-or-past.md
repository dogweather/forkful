---
title:                "Gleam: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Tulevaisuuden tai menneen päivän laskeminen on tärkeää monissa ohjelmoinnin projekteissa, kuten sovellusten suunnittelussa tai raporttien luomisessa.

## Kuinka

Voit käyttää Gleam-ohjelmointikieltä helposti laskemaan tietyn päivän tulevaisuudessa tai menneisyydessä. Seuraavassa on esimerkkejä koodista ja tulostuksesta Gleam-koodilohkoissa:

````Gleam
import Time

let tulevaisuuden_paiva = Time.add_days(Date.now(), 7)

let menneen_paiva = Time.add_days(Date.now(), -14)

println("Viikon kuluttua on päivämäärä: {}", tulevaisuuden_paiva)
println("Kaksi viikkoa sitten oli päivämäärä: {}", menneen_paiva)
```

Tämä koodi käyttää "Time" -moduulia, joka tarjoaa valmiita toimintoja päivämäärän laskemiseen. "add_days" -funktio ottaa parametreikseen nykyisen päivämäärän ja halutun määrän päiviä, jotka haluat lisätä tai vähentää. Lopuksi "println" -funktio tulostaa lasketun päivämäärän.

## Syvemmälle

Mikäli tarvitset tarkempaa laskentaa päivämäärille, voit käyttää "Calendar" -moduulia, joka tarjoaa enemmän toimintoja päivämäärien käsittelyyn. Voit esimerkiksi verrata kahta päivämäärää, tarkistaa onko se arkipäivä tai laskemalla määrän päiviä kahden päivämäärän välillä.

````Gleam
import Calendar

let tarkistus_paiva = Date.from_calendar(2021, 12, 31)

let tanaan = Date.now()

println("Onko {} arkipäivä? {}", tarkistus_paiva, Date.is_weekday(tarkistus_paiva))
println("Tänään on {} päivien päästä yhden vuoden kuluttua: {}", tanaan, Calendar.add_days(Date.add_years(tanaan, 1), 30))
```

Tämä koodi käyttää "Calendar" -moduulia vertaamaan antamaasi päivämäärää toimintoja käyttäen. Lisäksi voit lisätä päiviä haluamaasi päivämäärään käyttäen "add_days" -funktiota ja "Date.add_years" -funktiota, joka lisää vuoden nykyiseen päivämäärään.

## Katso Myös

- [Gleam-ohjelmointikielen viralliset kotisivut](https://gleam.run/)
- [Gleam-ohjelmointikielen dokumentaatio päivämäärän käsittelyyn](https://gleam.run/documentation/stdlib/date)
- [Esimerkkikoodit päivämäärän laskemiseen Gleamilla](https://github.com/gleam-lang/gleam/tree/main/examples/dates)