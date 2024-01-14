---
title:                "Ruby: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV-tiedostot ovat erittäin yleisiä tietomuotoja, jotka mahdollistavat tiedon tallentamisen ja jakamisen eri ohjelmien välillä. Jos työskentelet datan käsittelyn ja analysoinnin parissa, CSV-tiedostot ovat välttämättömiä työkaluja.

## Miten tehdä

Ruby-kielellä CSV-tiedostojen käsittely on helppoa ja nopeaa. Voit käyttää `csv`-kirjastoa, joka on osa Rubyn perusasennusta. Tässä esimerkissä luomme uuden CSV-tiedoston ja tallennamme siihen muutaman rivin dataa:

```Ruby
require 'csv' # sisällytä csv-kirjasto
CSV.open("tiedosto.csv", "w") do |csv| # avaa uusi tiedosto nimellä "tiedosto.csv"
	csv << ["Otsikko1", "Otsikko2", "Otsikko3"] # lisää otsikkorivin
	csv << ["Arvo1", "Arvo2", "Arvo3"] # lisää dataa ensimmäiselle riville
	csv << ["Arvo4", "Arvo5", "Arvo6"] # lisää dataa toiselle riville
end
```

Tässä koodissa käytetään `CSV.open`-metodia, joka avaa ja luo uuden CSV-tiedoston. Sitten käytetään `<<`-operaattoria lisäämään dataa tiedostoon. Lopputuloksena tiedostossa on kolme riviä, joissa on otsikkorivi ja kaksi data-riviä.

## Syvemmälle CSV:hen

CSV-tiedostojen käsittelyllä on laajempi pohja, joka kannattaa tutkia, jos aiot työskennellä usein CSV-tiedostojen kanssa. Voit esimerkiksi käsitellä tekstitiedostoja CSV-muotoon tai muuntaa CSV-tiedostoja taulukoiksi ja takaisin.

Tärkeää on myös huomata, että CSV-tiedostot eivät ole standardoituja ja niiden formaatti voi vaihdella. Siksi on hyvä tutustua tarkemmin CSV-standardiin ja sen mahdollisiin ongelmiin, kuten erikoismerkkeihin tai tyhjiin kenttiin.

## Katso myös

- [CSV-kirjaston dokumentaatio](https://ruby-doc.org/stdlib-2.6.3/libdoc/csv/rdoc/CSV.html)
- [CSV-standardin tiedot](https://tools.ietf.org/html/rfc4180)
- [Ruby-kirjasto tekstidatasta CSV-muotoon](https://github.com/tilo/smarter_csv)