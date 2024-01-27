---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? - "Mikä & Miksi?"
Työskenteleminen CSV-tiedostojen kanssa tarkoittaa niiden lukemista, kirjoittamista ja manipulointia. Ohjelmoijat tekevät tätä datan helpon siirtämisen ja analysoinnin vuoksi.

## How to: - "Kuinka tehdä:"
```Fish Shell
# CSV-tiedoston lukeminen
cat data.csv

# Sarakkeiden leikkaaminen ja niihin perustuva järjestäminen
cat data.csv | cut -d ',' -f 2,5 | sort

# CSV-tiedoston läpikäyminen ja tietyllä ehdolla filtteröiminen
awk -F, '$1 ~ /hakusana/ {print $0}' data.csv
```

Output esimerkki:
```
Juha, Meikäläinen, juha@example.com
Kaisa, Virtanen, kaisa@example.com
```

## Deep Dive - "Syväsukellus"
CSV-formaatti syntyi 1970-luvulla ja on yksinkertainen tekstitiedosto, jossa arvot on eroteltu pilkuilla. Vaihtoehtoja CSV:lle ovat esimerkiksi JSON ja XML. CSV-tiedostojen käsittely riippuu ohjelmointikielistä ja käytetyistä kirjastoista. Fish Shellissä yleisiä työkaluja ovat `cut`, `awk`, `sort` ja `grep`.

## See Also - "Katso Myös"
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Awk Manual](https://www.gnu.org/software/gawk/manual/gawk.html)
