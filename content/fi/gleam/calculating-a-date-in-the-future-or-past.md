---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Gleam: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on operaatio, johon liittyy nykyisen päivämäärän ja tietyn ajanvälin yhdistäminen. Ohjelmoijat tekevät tämän usein tapahtumien ajoittamiseen tai päätöksentekoprosessien aikatauluttamiseen.

## Miten Tehdään:
Tässä on Gleam-esimerkki siitä, kuinka laskea päivämäärä tulevaisuudessa:

```Gleam
import gleam/otp/erlang
import gleam/calendar.{Time}

fn main() {
    let nykyinen_aika = erlang.utc_time()
    let tuleva_aika = calendar.add_days(nykyinen_aika, 7)
    erlang.io.format("~p\n", [tuleva_aika])
}
```

Tämä ohjelma palauttaa nykyisen päivämäärän lisättynä seitsemällä päivällä:

```Gleam
"2022-12-10"
```

## Sukellus Syvään:
Historiallisesti päivän laskeminen tulevaisuuteen tai menneisyyteen oli monimutkainen prosessi, joka vaati monia ajopäiviä ja usein käsillä olevia kalentereita. Nykyään, Gleam ohjelmointikieli tekee siitä helppoa.

Vaihtoehtona on käyttää ohjelmointikielen sisäänrakennettua date-metodia, mutta Gleamin kalenterin lisäysmetodit tarjoavat paremman tavan.

Päivämäärän laskemista varten Gleam hyödyntää tp:n (Tuple Protocol) dynaamisesti tyypitettyä ominaisuutta. Se mahdollistaa tiedon säilyttämisen monimutkaisissa rakenteissa, joka on hyödyllistä päivämäärälaskennassa.
    
## Katso Myös:
Tutustu Gleam-dokumentaatioon tietääksesi enemmän Gleamin päivämäärä- ja aikafunktioista:
- [Erlang-otp](https://erlang.org/doc/man/erlang.html#type-time)