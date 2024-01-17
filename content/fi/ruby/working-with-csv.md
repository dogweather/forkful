---
title:                "Töitä tehdessä CSV:n kanssa"
html_title:           "Ruby: Töitä tehdessä CSV:n kanssa"
simple_title:         "Töitä tehdessä CSV:n kanssa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

CSV eli "comma-separated values" on tiedostomuoto, jota käytetään taulukkomuotoisten tietojen tallentamiseen. Ohjelmoijat käyttävät CSV:tä, koska se on helppo lukea ja käsitellä tietoja taulukkomuodossa.

## Miten:

Käyttäen Rubya voit lukea CSV-tiedostoja helposti ```CSV.read('tiedostonimi.csv')```. Tämä palauttaa tietojoukon, joka on tallennettu taulukkona. Voit myös tallentaa CSV-tiedostoja käyttäen ```CSV.open('tiedostonimi.csv', 'w') do |csv| csv << ["rivi1-solu1", "rivi1-solu2"] end```.

## Syväsukellus:

CSV on ollut käytössä jo vuodesta 1972 lähtien, mutta sen suosio on kasvanut viime vuosina XML-muodon rinnalla. CSV:tä käytetään usein tietojen siirtämisessä eri ohjelmistojen välillä. Vaihtoehtoina CSV:lle on esimerkiksi JSON-tiedostomuoto. CSV-tiedostoja käsitellään yleensä riveittäin, jolloin jokainen rivi vastaa yhtä tietueetta. CSV-tiedostojen kanssa voi myös käyttää erilaisia erottimia kuin vain pilkku, kuten puolipistettä tai tabulaattoria.

## Katso myös:

- [Ruby CSV Dokumentaatio](https://docs.ruby-lang.org/en/2.6.0/CSV.html)
- [Ruby CSV Hakemisto](https://ruby-doc.org/stdlib-2.6.0/libdoc/csv/rdoc/index.html)
- [Wikipedia: CSV](https://en.wikipedia.org/wiki/Comma-separated_values)