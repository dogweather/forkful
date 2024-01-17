---
title:                "Kahden päivämäärän vertailu"
html_title:           "Gleam: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Vertaamalla kahta päivämäärää ohjelmoijat voivat tarkistaa, ovatko kaksi tapahtumaa tapahtuneet samana päivänä tai kumpi niistä tapahtui ensin. Tämä on hyödyllistä esimerkiksi aikajärjestyksessä tapahtuvien tapahtumien seuraamisessa tai päivämäärän valinnassa järjestelmässä. 

## Miten:

Vertailun suorittamiseksi voit käyttää `Date`-moduulia ja sen `equal()` tai `compare()` -funktioita, jotka ottavat vastaan kaksi päivämäärää parametreina. Esimerkiksi:

```
Gleam.Date.equal(#date(2021, 12, 25), #date(2022, 1, 1))
// Palauttaa "false"
Gleam.Date.compare(#date(2020, 6, 15), #date(2020, 6, 10))
// Palauttaa "Gt" (greater than)
```
## Syventävä tarkastelu:
Päivämäärien vertailu on ollut haaste ohjelmointikielistä riippumatta. Usein virheelliset päivämäärämuodot tai aikavyöhykkeet voivat aiheuttaa ongelmia. On myös olemassa muita tapoja vertailla päivämääriä, kuten Unix-ajan käyttäminen. Gleam tarjoaa kuitenkin helppokäyttöiset muokkaimet ja avustajat päivämäärien kanssa työskentelyyn.

## Katso myös:
Lisätietoa päivämäärien vertailusta Gleamissa löytyy virallisesta dokumentaatiosta: https://gleam.run/documentation/library/date.html