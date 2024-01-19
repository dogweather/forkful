---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Ruby-ohjelmointi: Kaksi päivämäärää - Kuinka verrata?

## Mitä ja Miksi?

Päivämäärän vertailulla tarkoitamme kahden erillisen päivämäärän arvon vertaamista. Ohjelmoijat tarvitsevat sitä esimerkiksi päätelläkseen, onko jokin tapahtuma jo tapahtunut vai tapahtuuko se tulevaisuudessa.

## Kuinka tehdä:
Rubyssa päivämäärän vertailu on melko suoraviivaista. Katsotaan esimerkkiä. Voit verrata niitä kuten vertaat lukuja.

```ruby
require 'date'

date1 = Date.new(2000, 1, 1)
date2 = Date.new(2020, 1, 1)

if date1 > date2
  puts "date1 on myöhempi"
elsif date1 < date2
  puts "date2 on myöhempi"
else
  puts "Päivämäärät ovat samat"
end
```

Tämä tulostaa "date2 on myöhempi", koska 2020 on myöhempi kuin 2000.

## Syvällisempi tutkimus
Ruby tarjoaa `Date`-luokan päivämäärän vertailuun ja käsittelyyn. Tämä luokka on osa Ruby-kielen standardikirjastoa, ja se lanseerattiin Ruby 1.9:ssa, joten se on melko uusi ominaisuus.

Muissa ohjelmissa voidaan käyttää myös aikaleimaa vertailuun. Kuitenkin, Ruby-kielen `Date`-luokka on hyvin helppokäyttöinen ja sitä suositellaan, jos aikaleiman tai kellonajan käsittelyä ei tarvita. 

Huomaa, että päivämäärän vertailu itsessään on yksinkertainen operaatio, mutta asioita voi monimutkaistua, kun aikavyöhykkeet ja kellonajat otetaan huomioon.

## Katso myös:
- Ruby's Date luokan virallinen dokumentaatio: https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html
- Ruby ohjelmanoitikielen oppaat ja resurssit: https://www.ruby-lang.org/fi/documentation/
- Ruby-ohjelmointikielen syvempi tutkimus päivämäärän ja ajan käsittelystä: https://learn.co/lessons/ruby-advanced-class-methods-readme