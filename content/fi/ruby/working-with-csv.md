---
title:                "Työskentely csv-tiedostojen kanssa"
html_title:           "Ruby: Työskentely csv-tiedostojen kanssa"
simple_title:         "Työskentely csv-tiedostojen kanssa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi työskennellä CSV:n parissa? No, ensinnäkin CSV (Comma Separated Values) on yksi yleisimmistä tiedostomuodoista, jota käytetään tietojen tallentamiseen ja jakamiseen. Se on helppo lukea ja käsitellä, joten se on suosittu valinta monissa ohjelmointitehtävissä.

## Miten

```Ruby
# Avaaminen ja lukeminen
require 'csv'

CSV.foreach("tiedosto.csv") do |row|
  puts row # tulostaa jokaisen rivin taulukkona
end

# Kirjoittaminen
CSV.open("uusi_tiedosto.csv", "w") do |csv|
  csv << ["Nimi", "Sähköposti", "Puhelinnumero"] # lisää ensimmäisen rivin otsikot
  csv << ["Matti Meikäläinen", "matti@mail.com", "0401234567"] # lisää tietueen
end

# Järjestäminen
CSV.foreach("tiedosto.csv", headers: true).sort_by { |row| row["Nimi"] } do |row|
  puts row # tulostaa järjestetyn tiedoston taulukkona
end
```

Esimerkkituloste:

```
["Nimi", "Sähköposti", "Puhelinnumero"]
["Matti Meikäläinen", "matti@mail.com", "0401234567"]
["Emilia Esimerkki", "emilia@mail.com", "0509876543"]
["Teppo Testaaja", "teppo@mail.com", "0452468135"]
```

## Syvempää sukeltamista

CSV:n käsittelyyn liittyy monia hyödyllisiä toimintoja, kuten tietojen muokkaaminen, poimiminen ja yhdistäminen. Voit myös käyttää taulukkoja tai rakenteita CSV-tiedostojen sijaan. Lisäksi CSV-kirjastossa on monia muita parametreja, joiden avulla voit muokata toimintaa.

### Tiedostojen lukeminen ja kirjoittaminen

CSV-kirjasto tarjoaa paljon vaihtoehtoja tiedostojen käsittelyyn. Voit esimerkiksi määrittää erottimen, käyttää otsikkoja tai jopa määrittää omia muotoiluja tiedostolle.

### Tiedostojen järjesteleminen

Käyttämällä `sort_by` -metodia voit järjestää CSV-tiedoston rivejä haluamasi kentän perusteella. Voit myös käyttää `reverse` -metodia, jos haluat kääntää järjestyksen.

## Katso myös

- [Ruby:n virallinen CSV-dokumentaatio](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)
- [CSValot - CSV:n käsittelyyn tarkoitettu Ruby-kirjasto](https://github.com/kmaida/csvalot)
- [Ruby:n perusteet - CSV-tiedostojen käsittely](https://www.tutorialspoint.com/ruby/ruby_input_output.htm#csvfiles)