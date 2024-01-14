---
title:    "Gleam: Päivämäärän laskeminen tulevaisuudesta tai menneisyydestä"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi: Miksi laskisi tulevaisuuden tai menneisyyden päivämäärää?

Monissa tilanteissa voi olla tarpeen laskea tulevaisuuden tai menneisyyden päivämäärä, kuten tietyn tapahtuman ajankohta tai tietyn ajanjakson laskeminen. Gleamin Date-moduuli tarjoaa helpon tavan tehdä tällaisia laskelmia.

## Miten: Esimerkkejä ja tulosteita koodia käyttäen

Jos haluat laskea tietyn päivämäärän tietyn päivien määrän verran eteenpäin, voit käyttää ```Date.from_days(aloitus_päivä, päivien_määrä)``` -funktiota. Esimerkiksi, jos aloitus_päivä on 1. tammikuuta 2022 ja haluat laskea 10 päivää eteenpäin, koodi olisi: 

```Gleam
Date.from_days(1.january(2022), 10)
```

Tämä tulostaisi 11. tammikuuta 2022. Voit myös laskea päivämäärän tietyn ajanjakson verran taaksepäin käyttämällä ```Date.substract_days(päivämäärä, päivien_määrä)``` -funktiota. Esimerkiksi, jos haluat laskea 20 päivää taaksepäin 1. maaliskuuta 2022, koodi olisi:

```Gleam
Date.substract_days(1.march(2022), 20)
```

Tämä tulostaisi 9. helmikuuta 2022.

## Syvempi sukellus: Lisätietoa tulevaisuuden ja menneisyyden päivämäärien laskemisesta

Gleamin Date-moduuli tarjoaa myös muita hyödyllisiä funktioita, kuten ```Date.is_leap(year)``` -funktion, joka tarkistaa onko kyseessä karkausvuosi. Voit myös käyttää ```Date.diff(date1, date2)``` -funktiota laskeaksesi päivien määrän kahden päivämäärän välillä.

## Katso myös

- Gleamin virallinen ohjelmointikielen tietosivusto: https://gleam.run/
- Gleam Date-moduulin dokumentaatio: https://gleam.run/modules/date.html
- Gleam-yhteisön keskustelufoorumi: https://elixirforum.com/c/gleam/25