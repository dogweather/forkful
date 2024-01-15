---
title:                "Csv-tiedostojen käsittely"
html_title:           "Bash: Csv-tiedostojen käsittely"
simple_title:         "Csv-tiedostojen käsittely"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat käsitellä taulukkomuotoisia tietoja, kuten Excel-tiedostoja, CSV-formaatti (comma-separated values) on erittäin kätevä. CSV-tiedostot ovat helppolukuisia ja niitä voidaan käyttää eri ohjelmistoissa ja kielissä. Bash-ohjelmointikielellä voit helposti käsitellä ja muokata CSV-tiedostoja, joten tämä taito voi olla hyödyllinen monissa tilanteissa.

## Miten

Bashilla on valmiita työkaluja CSV-tiedostojen käsittelyyn, ja voit myös luoda omia skriptejä tarpeidesi mukaan. Tässä ovat esimerkkejä tyypillisistä CSV-tiedostojen käsittelytoiminnoista, joita voit suorittaa Bashilla.

```Bash
# Näytä CSV-tiedoston sisältö terminaalissa
cat tiedosto.csv

# Järjestä tiedoston sisältö aakkosjärjestykseen ja tallenna uuteen tiedostoon
sort -t ',' -k 2 tiedosto.csv > jarjestetty.csv

# Hae tiettyjä rivejä tiedostosta ja tallenna ne uuteen tiedostoon
grep "Helsinki" tiedosto.csv > kaupunki.csv

# Laske tiedoston rivien määrä
wc -l tiedosto.csv
```

Näiden lisäksi Bashilla voi suorittaa monia muita toimintoja, kuten tiedostojen yhdistämistä, muuttamista ja suodattamista. Suosittelemme kokeilemaan erilaisia komentoja ja tutustumaan Bashin CSV-työkaluihin lisää.

## Syvä sukellus

CSV-tiedostot sisältävät yleensä vain yksinkertaista tekstiä, joten niitä on helppo käsitellä Bashilla. Voit kuitenkin kohdata haasteita, jos tiedostossa on muuttuvia sarakkeita tai jos sarakkeiden välissä on muita erottimia kuin pilkku. Tässä tilanteessa voit käyttää Basken "IFS" -muuttujaa, joka määrittelee, mitkä merkit erottavat sarakkeet toisistaan.

Lisäksi voit käyttää Bashin silmukka-rakennetta (for, while) käymään läpi tiedoston rivit ja suorittamaan erilaisia toimintoja jokaiselle riville. Tämä on erityisen hyödyllistä, jos CSV-tiedostossa on paljon dataa ja haluat käsitellä sitä rivi kerrallaan.

## Katso myös

- Bashin "read" -komennon käyttö CSV-tiedostojen lukemiseen: https://bash.cyberciti.biz/guide/Reads_a_CSV_file_(values_separated_by_commas)
- Bashin "cut" -komennon käyttö tiettyjen sarakkeiden erottamiseen CSV-tiedostosta: https://www.tutorialkart.com/bash/shell-cut-command-examples-csv-files/
- Bashin "awk" -komennon käyttö CSV-tiedostojen käsittelyyn ja muokkaamiseen: https://www.codingdefined.com/2014/12/awk-command-to-read-csv-file.html