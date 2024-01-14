---
title:    "Gleam: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa on tarve laskea tietty päivämäärä tulevaisuudessa tai menneisyydessä. Tämän voi tehdä helposti Gleam-ohjelmointikielellä.

## Kuinka

Gleamissa käytetään `gleam/time`-kirjastoa päivämäärien käsittelyyn. Voit aloittaa käyttämällä päivämäärän luomiseen tarvittavia funktioita ja antamalla niille halutut päivämäärät tai ajanjaksoja.

```Gleam
import time

// Luodaan tuleva päivämäärä 14 päivää nykyisestä päivästä
let future_date = time.add_days(time.now(), 14)

// Luodaan menneisyyden päivämäärä 7 päivää nykyisestä päivästä
let past_date = time.subtract_days(time.now(), 7)

// Tulostetaan luodut päivämäärät
time.format(future_date, "d.m.YYYY") // 30.08.2021
time.format(past_date, "d.m.YYYY") // 09.08.2021
```

Gleam tarjoaa myös muita funktioita, kuten `add_months` ja `subtract_months`, joiden avulla päivämääriin voi lisätä tai vähentää kuukausia.

## Syvemmälle sukeltaminen

Päivämäärien laskeminen tulevaisuudessa tai menneisyydessä voi tuntua aluksi vaikealta, mutta Gleamin `gleam/time`-kirjasto tekee siitä helpompaa. Kirjastosta löytyy myös muita hyödyllisiä funktioita, kuten `is_leap_year`, jolla voi tarkistaa onko vuosi karkausvuosi, sekä `is_before` ja `is_after`, jotka auttavat vertailemaan päivämääriä.

On myös hyvä tietää, että Gleamissa päivämäärät ovat oliona, eli ne eivät ole muuttumattomia. Tämä tarkoittaa sitä, että niitä voi muokata käyttämällä erilaisia funktioita ja metodinviauttajia.

## Katso myös

- Gleam `gleam/time` dokumentaatio: https://gleam.run/modules/time.html
- Gleamin oppaat: https://gleam.run/book/index.html