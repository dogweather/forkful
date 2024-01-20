---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hae päivämäärää Gleamilla

## Mikä & Miksi?
Päivämäärän hakeminen viittaa siihen, kun ohjelmoija saa selville nykyisen päivämäärän. Sitä tarvitaan useissa sovelluksissa, kuten lokienseurannassa, aikaleimojen luomisessa, tai dynaamisissa tehtävissä, jotka perustuvat päivän aikaan.

## Näin se tehdään:
Gleam-ohjelmointikielessä päivämäärän hakeminen tapahtuu seuravasti:

```gleam
import gleam/locale.{month, second, day, year}
import gleam/date_time.now

fn main() {
  let nyt = now()
  let paiva = day(now)
  let kuukausi = month(now)
  let vuosi = year(now)
  let sekunti = second(now)
  
  io.println("Tämän hetkinen päivämäärä on " ++ paiva ++ "/" ++ kuukausi ++ "/" ++ vuosi)
  io.println("Tämän hetkinen sekunti on " ++ sekunti)
}
```
Se tulostaa nykyisen päivämäärän ja sekunnin.

## Syvempi sukellus
Historiallisesti päivämäärän hakeminen on ollut tärkeä osa ohjelmointia ja sen merkitys on vain kasvanut. Gleam tarjoaa selkeän ja intuitiivisen tavan hakea nykyistä päivämäärää ja aikaa. 

Vaihtoehtoisesti, voit käyttää myös muita Gleam-kirjastoja kuten `time`-kirjastoa. Kummassakin tapauksessa, päivämäärä ja aika lasketaan senhetkisen järjestelmän ajasta, jossa koodi suoritetaan.

Gleamissa päivämäärän haku perustuu Erlangin kerrosalustaan, joka antaa hyvän tarkkuuden ja suorituskyvyn.

## Katso myös
1. Gleam's DateTime dokumentaatio: [https://hexdocs.pm/gleam_datetime/readme.html](https://hexdocs.pm/gleam_datetime/readme.html)
2. Gleam's Locale dokumentaatio: [https://hexdocs.pm/gleam_locale/readme.html](https://hexdocs.pm/gleam_locale/readme.html)
3. Gleamin opas: [https://gleam.run/book/tour/dates-and-time.html](https://gleam.run/book/tour/dates-and-time.html)
4. Erlangin DateTime-moduuli: [https://erlang.org/doc/man/calendar.html](https://erlang.org/doc/man/calendar.html)