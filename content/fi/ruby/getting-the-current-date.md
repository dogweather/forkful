---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

"Getting the current date" on ohjelmointitermi, joka tarkoittaa tietokonejärjestelmän nykyisen päivämäärän hankkimista. Ohjelmoijat tarvitsevat sitä vastaamaan moniin käyttäjän tarpeita, kuten lokitiedostojen merkintöjen aikaleimoja varten.

## Näin teet:

Ruby tarjoaa `Time`-luokan, jolla voit helposti hankkia nykyisen päivämäärän. Koodaamme esimerkin:

```Ruby
nykyinen_pvm = Time.now
puts nykyinen_pvm.strftime("%d/%m/%Y")
```

Suoritettaessa saat vastauksen muodossa `DD/MM/YYYY`, esimerkiksi: `19/03/2022`.

## Syvempi sukellus:

Historiallinen tausta: Kaikki alkoi silloin kun tietokoneet ja ohjelmistot alkoivat tarvita päivämäärää ja aikaa tehtävien suoritusajan seurannan ja aikaleimojen perusteella. Ruby otti käyttöön `Time` luokan helpottamaan tätä tarvetta.

Vaihtoehtoja: Rubyssa voit myös käyttää `Date.today` saadaksesi vain nykyisen päivämäärän. On olemassa myös muita gemmejä, kuten `chronic`, jotka voivat tarjota lisätoimintoja.

Toteutuksen yksityiskohdat: `Time.now` palauttaa ajan, joka sisältää nykyisen päivämäärän sekä kellonajan. `strftime` on metodi, joka muotoilee ajan käyttäjän haluamaksi päivämääräksi.

## Katso myös:

Linkkejä, joista saat lisätietoja:

- Ruby-dokumentaatio: [Time-luokka](https://ruby-doc.org/core-2.7.0/Time.html)
- Ruby-dokumentaatio: [Date-luokka](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- [Ruby Basics: Päivämäärät ja ajat](https://www.rubyguides.com/ruby-tutorial/dates-times/), RubyGuides.