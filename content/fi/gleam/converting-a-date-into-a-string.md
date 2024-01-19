---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi & Mitä?
Muuntaa päivämäärä merkkijonoksi tarkoittaa päivämäärädatan esittämistä ihmisen luettavassa muodossa. Tätä tarvitaan usein, kun päivämäärätiedot esitetään käyttöliittymissä tai tallennetaan tekstitiedostoihin.

## Näin teet:
Gleam-kielessä voit muuntaa päivämäärän merkkijonoksi käyttämällä `date.to_string` -funktiota. Tässä on esimerkkikoodi:

```gleam
import gleam/date

fn main() {
  let date = date.new(2022, 12, 24)
  let date_string = date.to_string(date)
  io.println(date_string)
}
```
Tämä tulostaa: "2022-12-24"

## Suurennuslasi
Päivämäärän muuntamistoiminto merkkijonoksi on ollut olemassa ohjelmointikielissä jo pitkään, koska se on välttämätön toiminto monissa sovelluksissa. Gleam tarjoaa tämän toiminnon `date.to_string`-funktion avulla ja se noudattaa ISO 8601 -standardin mukaista päivämäärän muotoa (YYYY-MM-DD).

Vaihtoehtoisesti voit käyttää muita kirjastoja tai manuaalisesti muotoilla päivämäärän haluamallasi tavalla, mutta se on usein monimutkaisempi ja virhealttiimpi tapa.

Tämän toiminnon yksityiskohtaisempi toteutus jakautuu kahteen osaan: päivämääräluonti itsessään, joka tapahtuu `date.new` -funktion avulla, ja päivämäärän muuntaminen merkkijonoksi `date.to_string` -funktion avulla.

## Katso myös:
Voit lukea lisää Gleam's `date`-moduulista ja sen tarjoamista funktioista täältä: [Gleam Date Documentation](https://hexdocs.pm/gleam_stdlib/gleam/date.html)
Samoin, lisätietoa päivämäärämuodosta (ISO 8601) löydät täältä: [ISO 8601 Wikipedia](https://fi.wikipedia.org/wiki/ISO_8601)