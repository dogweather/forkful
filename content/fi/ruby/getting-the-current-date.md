---
title:                "Nykyisen päivämäärän hakeminen"
html_title:           "Ruby: Nykyisen päivämäärän hakeminen"
simple_title:         "Nykyisen päivämäärän hakeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Nykyisen päivämäärän hakeminen on yksinkertainen tapa saada tarkka ja luotettava ajanilmaisu, jota voidaan käyttää erilaisissa ohjelmoinnin projekteissa. Tämä on erityisen tärkeää ohjelmien, jotka kuuluvat erilaisiin aikavyöhykkeisiin tai käsittelevät aikaperusteisia toimintoja, kehittämisessä.

## Miten:
```Ruby
Time.now
```
Esimerkki koodista palauttaa nykyisen ajan ja päivämäärän muodossa "2021-10-08 12:45:00 +0300". Tämä voidaan tallentaa muuttujaan ja käyttää myöhemmin ohjelmassa. 

## Syvemmällä:
Nykyisen päivämäärän hakeminen on ollut tärkeä osa ohjelmointia jo pitkään. Ennen tätä toimintoa, ohjelmoijien piti itse kirjoittaa algoritmeja laskeaakseen nykyisen päivämäärän ja ajan. On myös olemassa muita tapoja saada nykyinen päivämäärä, kuten käyttämällä ulkoisia kirjastoja, mutta Rubyssa tämä toiminto on integroituna ja helppokäyttöinen.

## Katso myös:
- [Ruby Time-luokka](https://ruby-doc.org/core-3.0.2/Time.html)
- [Date and Time -kirjasto Rubyssa](https://apidock.com/ruby/DateTime)
- [Ruby Ohjelmointikielen Viralliset Sivut](https://www.ruby-lang.org/fi/)