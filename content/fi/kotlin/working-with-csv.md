---
title:                "Työskentely csv-tiedostojen kanssa"
html_title:           "Kotlin: Työskentely csv-tiedostojen kanssa"
simple_title:         "Työskentely csv-tiedostojen kanssa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

Mikä & Miksi?

CSV, eli Comma Separated Values, on tiedostomuoto, jota käytetään datan tallentamiseen ja jakamiseen taulukkomuodossa. CSV-tiedosto koostuu eri sarakkeista, jotka ovat erotettu toisistaan pilkulla. Ohjelmoijat käyttävät CSV-tiedostoja usein, koska ne ovat helposti luettavissa ja muokattavissa.

Kuinka tehdä:

Esimerkiksi, jos haluat luoda CSV-tiedoston, jossa on kolme saraketta, nimeltään "nimi", "ikä" ja "kaupunki", voit käyttää seuraavaa koodia:

```Kotlin
val data = listOf(
    listOf("Matti", "25", "Helsinki"),
    listOf("Anna", "30", "Tampere"),
    listOf("Tuomas", "20", "Turku")
)

val csv = data.joinToString("\n") { it.joinToString() }

println(csv)

```

Tämä koodi tulostaa seuraavanlaisen CSV-tiedoston:

```
nimi,ikä,kaupunki
Matti,25,Helsinki
Anna,30,Tampere
Tuomas,20,Turku
```

Syvemmälle upottautuminen:

CSV-tiedostomuoto on ollut olemassa jo vuosikymmeniä ja se on edelleen suosittu tiedostomuoto, koska se on yhteensopiva monien eri ohjelmistojen kanssa. Alternatiivina CSV:lle on esimerkiksi XML-tiedostomuoto, jossa data tallennetaan hierarkkisessa muodossa. CSV-tiedostojen käsittelyyn on myös olemassa erilaisia kirjastoja, kuten esimerkiksi Apache Commons CSV ja OpenCSV.

Katso myös:

Jos haluat lisätietoja CSV-tiedostoista ja niiden käsittelystä Kotlinilla, löydät hyödyllistä tietoa seuraavista lähteistä:

- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-input-stream/buffered-reader.html)
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/)
- [OpenCSV](http://opencsv.sourceforge.net/)