---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Päivämäärien vertaaminen on prosessi, jossa kaksi eri päivämäärää sidotaan vertailuoperaattorin kautta. Ohjelmoijat tekevät tämän esimerkiksi päivämäärien aikajärjestyksen määrittämiseksi tai aikaerotasojen laskemiseksi.

# Miten Tehdään:

```
// Tuodaan tarvittavat Gleam-kirjastot
import gleam/calendar.{calendar, datetime}
import gleam/int.{int}
import gleam/bool.{bool}
import gleam/order.{order}

// Luodaan kaksi päivämäärää
let day1 = calendar.local_date(2020, 12, 27)
let day2 = calendar.local_date(2022, 8, 3)

// Vertaillaan päivämääriä
let are_equal = datetime.equal(day1, day2) // False
let is_before = datetime.before(day1, day2) // True
let is_after = datetime.after(day1, day2) // False
```
# Syvä Sukellus:

Päivämäärien vertailu ei ole mikään uusi konsepti, se on ollut tärkeä osa ohjelmointia sen varhaisista vaiheista lähtien. Useimmat ohjelmointikielet, mukaan lukien Gleam, tarjoavat sisäänrakennettuja työkaluja päivämäärien vertailuun.

On erilaisia tapoja vertailla päivämääriä. Tässä artikkelissa keskitymme järjestysvertailuihin (ennen, jälkeen, tai yhtä suuri). Kuitenkin, voit myös vertailla päivämäärien välisiä aikaeroja, jos vertailtavat päivämäärät ovat esimerkiksi aikaleimoja.

Vertailuoperaattorit Gleamissa antavat joko totuusarvoja (`true` tai `false`) tai hätäkoodeja antaen ilmoituksen ohjelmistolle, jos jotain menee pieleen päivämääriä luodessa. Kuitenkin, Gleamin `gleam/calendar`-kirjasto voit luottaa, että se luo päivämäärät oikein.

# Katso Myös:

Jos haluat syventää tietojasi Gleamin päivämäärien vertailusta, katso seuraavat lähteet:

1. Gleam Documentation: [https://gleam.run/docs/introduction/](https://gleam.run/docs/introduction/)
2. Gleam GitHub: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam) 
3. Gleam Date & Time Library: [https://gleam.run/stdlib/datetime.html](https://gleam.run/stdlib/datetime.html)