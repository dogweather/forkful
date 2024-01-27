---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, eli Comma-Separated Values, on tiedostomuoto, jossa data on eroteltu pilkuilla. Ohjelmoijat käyttävät CSV-tiedostoja, koska niiden käsittely on helppoa ja ne ovat yleisiä datanvaihdossa.

## How to:
Gleamilla tiedoston lukeminen ja kirjoittaminen käy näppärästi. Tässä esimerkki CSV-tiedoston läpikäynnistä ja uuden tiedoston luonnista.

```Gleam
// CSV-tiedoston lukeminen
external fn read_csv(file_path: String) -> Result(List(List(String)), String)

// CSV-tiedoston kirjoittaminen
external fn write_csv(file_path: String, data: List(List(String))) -> Result(Nil, String)
```

Sample output:
```
Ok([["nimi", "ikä"], ["Matti", "30"], ["Liisa", "25"]])
```

## Deep Dive
CSV on vanha tiedostomuoto, mutta edelleen suosittu sen yksinkertaisuuden vuoksi. Vaihtoehtoisesti voisi käyttää JSONia tai XML:ää, joilla on monipuolisemmat rakenteet. CSV:n käsittely Gleamissa perustuu ulkoisiin funktioihin, joita voi kutsua Gleamin standardikirjaston avulla.

## See Also
- CSV:stä yleisesti: [https://en.wikipedia.org/wiki/Comma-separated_values](https://en.wikipedia.org/wiki/Comma-separated_values)

Huomaa, että tässä artikkelissa esitetyt `external fn` esimerkit ovat hypoteettisia ja oletuksena on, että sinulla on käytössäsi ulkoinen kirjasto CSV:n käsittelyyn Gleamissa.
