---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"

category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why - Mitä ja Miksi?
CSV on yksinkertainen tiedostomuoto tallentaa taulukollista tietoa, kuten Excel-tiedostoissa. Ohjelmoijat käyttävät sitä sen helpon muokattavuuden ja yhteensopivuuden eri järjestelmien välillä takia.

## How to - Miten?
```Bash
# Lue CSV-tiedosto ja tulosta sen sisältö
while IFS=, read -r kentta1 kentta2 kentta3
do
  echo "Kenttä1: $kentta1 - Kenttä2: $kentta2 - Kenttä3: $kentta3"
done < tiedosto.csv

# Lisää rivi CSV-tiedostoon
echo "arvo1,arvo2,arvo3" >> tiedosto.csv

# Suodata ja tulosta vain tietyt sarakkeet käyttäen cut-komentoa
cut -d',' -f2 tiedosto.csv
```

Esimerkki tuloste:
```
Kenttä1: arvo1 - Kenttä2: arvo2 - Kenttä3: arvo3
```

## Deep Dive - Syväsukellus
CSV:ää on käytetty jo ennen tietokoneiden aikakautta. Se on yksinkertainen mutta joustava, ja monet ohjelmat tukevat sitä. Vaihtoehtoisia tiedostomuotoja ovat esimerkiksi JSON ja XML, mutta niitä on monimutkaisempi käsitellä. Bash-käsittelyssä tärkeää on ymmärtää symbolien, kuten IFS:n (Internal Field Separator), merkitys ja toiminta.

## See Also - Katso Myös
- [GNU Coreutils](https://www.gnu.org/software/coreutils/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
