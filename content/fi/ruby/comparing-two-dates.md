---
title:                "Vertailla kahden päivämäärän välillä"
html_title:           "Ruby: Vertailla kahden päivämäärän välillä"
simple_title:         "Vertailla kahden päivämäärän välillä"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärien vertailu on yksinkertainen prosessi, jossa verrataan kahta eri päivämäärää keskenään. Ohjelmoijat tekevät tätä esimerkiksi tarkistaessaan, kumpi päivämäärä on suurempi tai jos kaksi päivämäärää on sama.

## Kuinka:

```Ruby
date1 = Date.new(2021, 1, 25)
date2 = Date.new(2021, 1, 20)

puts date1 > date2 # tulos: true
puts date1 == date2 # tulos: false
```

## Syvän sukelluksen

Päivämäärien vertailua on käytetty ohjelmoinnissa jo pitkään. Usein siinä käytetään ohjelmointikielen tarjoamia valmiita toimintoja, kuten Rubyssa olevaa > ja == operaattoria. On myös olemassa muita tapoja vertailla päivämääriä, kuten muuntamalla ne UNIX-aikaleimoiksi ja vertailemalla niitä. Tämä voi olla kätevää, jos halutaan tarkastella päivämääriä tarkemmin.

## Katso myös

Lisätietoa päivämäärien vertailusta ja vaihtoehtoisia tapoja tehdä se löydät seuraavista lähteistä:

* [Ruby Date-luokka](https://ruby-doc.org/core-3.0.1/Date.html)
* [Unix aikaleimat](https://en.wikipedia.org/wiki/Unix_time)