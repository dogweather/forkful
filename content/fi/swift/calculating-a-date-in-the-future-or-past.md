---
title:                "Swift: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa, kuten sovellusten ja verkkosivujen kehityksessä, saatamme joutua käsittelemään päivämääriä. Joskus meidän on ehkä laskettava päivämäärä tulevaisuudesta tai menneisyydestä. Tämä voi olla hyödyllistä esimerkiksi laskutamme tilauksia tai näytämme käyttäjälle tietyn päivämäärän mukaan muuttuvia tietoja. Tässä blogikirjoituksessa opimme, miten voimme käyttää Swiftin date-olioita tulevien tai menneiden päivämäärien laskemiseen.

## Miten tehdä

Date-oliot ovat tärkeitä osia Swiftin Foundation Frameworkista. Ne mahdollistavat päivämäärien ja aikojen käsittelyn ja muuntamisen eri muodoissa. Tässä seuraavassa esimerkissä näytämme, miten voimme käyttää date-olioita laskeaksemme päivämäärän tietyn määrän päiviä eteen- tai taaksepäin.

```Swift
let today = Date()
let futureDate = Calendar.current.date(byAdding: .day, value: 7, to: today)
let pastDate = Calendar.current.date(byAdding: .day, value: -7, to: today)
print("Tänään on \(today) ja viikon päästä on \(futureDate ?? "ei saatavilla"). Menneisyydessä puolestaan oli \(pastDate ?? "ei saatavilla").")

```

Tämä koodi käyttää `Calendar`-luokkaa ja sen `date(byAdding:to: )` -metodia lisäämään tai vähentämään halutun määrän päiviä annettuun päivämäärään. Tässä tapauksessa annamme parametrina `day`, joka merkitsee lisättäviä/vähennettäviä päiviä, ja antamamme arvo määrää, kuinka monta päivää haluamme lisätä tai vähentää. Tulostus näyttää meille tänään, viikon päästä ja viikko sitten olevat päivämäärät.

```
Tänään on 2021-10-15 08:00:00 +0000 ja viikon päästä on 2021-10-22 08:00:00 +0000. Menneisyydessä puolestaan oli 2021-10-08 08:00:00 +0000.
```

## Syvempi sukellus

Swiftin date-oliot ovat rakenteita, jotka sisältävät päivämäärän ja ajan tiedot tiettyyn aikavyöhykkeeseen suhteutettuna. Tämän ansiosta voimme käyttää niitä helposti päivämäärien muunnoksiin ja laskuihin.

Date-olioiden lisäksi Swift tarjoaa myös muita luokkia, kuten `DateFormatter` ja `Calendar`, jotka ovat hyödyllisiä eri päivämäärien muotoiluun ja käsittelyyn. On myös tärkeää huomata, että date-olioiden käsittelyssä on otettava huomioon myös aikavyöhykkeet ja mahdolliset kesäajat, jotta saamme halutun päivämäärän ilman virheitä. Siksi esimerkiksi `Calendar`-luokan avulla voimme määrittää tietyn aikavyöhykkeen käyttämiemme päivämäärien suhteen.

Toivottavasti tämä blogikirjoitus auttoi sinua ymmärtämään, miten voimme käyttää Swiftin date-olioita laskemaan tulevia tai menneitä päivämääriä. Muista aina huolellisesti käsitellä päivämääriä ja ottaa hu