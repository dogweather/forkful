---
title:                "Työskentely CSV:n kanssa"
aliases: - /fi/ruby/working-with-csv.md
date:                  2024-02-03T19:21:24.006792-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely CSV:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

CSV-tiedostojen käsittely Rubyssa tarjoaa yksinkertaisen tavan käsitellä taulukkomuotoista tietoa. Ohjelmoijat harjoittavat tätä käytäntöä usein datan jäsentämiseen, poimintaan, muuntamiseen ja tallentamiseen, mikä tekee siitä kriittisen taidon tehtävissä, jotka liittyvät datan manipulointiin tai analysointiin.

## Kuinka:

Ruby sisältää oletuksena CSV-kirjaston, joka yksinkertaistaa lukemista ja kirjoittamista CSV-tiedostoihin. Tässä on, miten voit hyödyntää tätä yleisiin tehtäviin:

### CSV-tiedoston lukeminen
CSV-tiedoston lukemiseksi sinun täytyy ensin vaatia CSV-kirjasto. Sen jälkeen voit iteroida rivejä tai lukea ne taulukkoon.

```ruby
require 'csv'

# Lukee jokaisen rivin taulukkona
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# Kunkin rivin tuloste voisi näyttää tältä: ["data1", "data2", "data3"]
```

### Kirjoittaminen CSV-tiedostoon
CSV-tiedostoon kirjoittaminen on myös suoraviivaista. Voit liittää olemassa olevaan tiedostoon tai luoda uuden tiedoston kirjoittamista varten.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["otsikko1", "otsikko2", "otsikko3"]
  csv << ["arvo1", "arvo2", "arvo3"]
end

# Tämä luo tai ylikirjoittaa 'output.csv':n määritetyillä otsikoilla ja arvoilla.
```

### CSV-merkkijonon jäsentäminen
Joskus sinun tarvitsee jäsentää CSV-data suoraan merkkijonosta. Tässä on miten:

```ruby
require 'csv'

data = "nimi,ikä,kaupunki\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['nimi']} - #{row['ikä']} - #{row['kaupunki']}"
end

# Odotettu tuloste:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### SmarterCSV:n käyttö
Monimutkaisemmissa CSV-tehtävissä `SmarterCSV`-gemmi voi olla arvokas työkalu. Asenna gemmi ensin:

```shell
gem install smarter_csv
```

Sitten voit käyttää sitä käsitellä isoja tiedostoja tai suorittaa kehittyneempiä jäsentämisen ja manipuloinnin tehtäviä:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# Tämä lukee 'large_data.csv':n ja tulostaa jokaisen rivin hashtaulukkona otsikoiden perusteella.
```

Yhteenvetona, Rubyn sisäänrakennettu CSV-kirjasto yhdessä kolmannen osapuolen gemmien kuten `SmarterCSV` kanssa tarjoaa vahvan tuen CSV-datan käsittelyyn, mahdollistaen tehokkaat datankäsittely- ja manipulointitehtävät.
