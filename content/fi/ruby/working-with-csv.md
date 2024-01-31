---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma-Separated Values) on tiedostoformaatti, joka tallentaa taulukollisen dataa yksinkertaisessa tekstiformaatissa. Ruby-ohjelmoijat käyttävät CSV:tä, koska se on laajasti tuettu, helppokäyttöinen ja se mahdollistaa datan siirtämisen eri järjestelmien ja ohjelmien välillä.

## How to:
```Ruby
require 'csv'

# CSV-tiedoston kirjoittaminen
CSV.open("esimerkki.csv", "w") do |csv|
  csv << ["Nimi", "Ikä", "Kaupunki"]
  csv << ["Sakari", 28, "Helsinki"]
  csv << ["Laura", 35, "Turku"]
end

# CSV-tiedoston lukeminen
CSV.foreach("esimerkki.csv", headers: true) do |row|
  puts "#{row['Nimi']} on kotoisin kaupungista #{row['Kaupunki']}."
end
```
Output:
```
Sakari on kotoisin kaupungista Helsinki.
Laura on kotoisin kaupungista Turku.
```

## Deep Dive
CSV-formaatti on ollut tiedonsiirron perustyökalu jo vuosikymmeniä. Se on tullut suosituksi sen yksinkertaisuuden ja joustavuuden vuoksi, ja se käy hyvin yhteen monien taulukkolaskentaohjelmien kanssa. Ruby-kielessä `csv`-kirjasto on standardikirjasto, joka on ollut sisäänrakennettuna jo jonkin aikaa ja mahdollistaa CSV-tiedostojen käsittelyn suoraviivaisesti. Vaihtoehtoisesti voit käyttää esimerkiksi `FasterCSV`-kirjastoa, joka tunnetaan nopeudestaan, mutta moderni Ruby käyttää `csv`-kirjastoa, koska se on yhdistetty `FasterCSV`:n kanssa.

## See Also
- Ruby Standard Library CSV: https://ruby-doc.org/stdlib-3.1.0/libdoc/csv/rdoc/CSV.html
- FasterCSV (nykyään osa Ruby Standard Library): https://github.com/JEG2/faster_csv
- Ruby-dokumentaatio: https://www.ruby-lang.org/fi/documentation/
