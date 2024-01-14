---
title:                "Kotlin: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi käyttää CSV-tiedostoja Kotlinilla?

CSV-tiedostot ovat yleinen tapa tallentaa ja vaihtaa taulukkomuotoisia tietoja, kuten tietokannan tietoja tai Excel-taulukoita. Koska Kotlin tarjoaa sujuvaa ja helppoa tapaa käsitellä tiedostoja, CSV-tiedostot ovat hyvä vaihtoehto tietojen tallentamiselle ja käsittelylle.

## Näin käytät CSV-tiedostoja Kotlinilla

CSV-tiedostoja voidaan lukea ja kirjoittaa käyttäen `java.io.File` ja `kotlinx.io` kirjastoja. Ensimmäisenä meidän täytyy määritellä tiedoston sijainti:

```Kotlin
val file = File("tiedosto.csv")
```

CSV-tiedostossa tietueet ovat jakautuneet pilkulla erillisiksi kentiksi ja rivit vaihdetaan. Voimme käyttää `useLines` metodia lukeaksemme kaikki rivit ja jakaa ne pilkun avulla:

```Kotlin
file.useLines { lines ->
    lines.forEach { line ->
        val items = line.split(",")
        println(items)
    }
}
```

Jos haluamme kirjoittaa CSV-tiedostoon, voimme käyttää `PrintWriter` ja `forEachLine` metodia:

```Kotlin
val pw = PrintWriter(file)

var data = listOf("John,32", "Jane,28", "Matt,40")

data.forEach {
    pw.println(it)
}

pw.close()
```

## Syvemmälle CSV-tiedostojen käsittelyyn

CSV-tiedostojen käsittelyyn on olemassa myös monia hyödyllisiä kirjastoja, kuten `com.opencsv`. Tämä kirjasto tarjoaa sujuvan tavan lukea ja kirjoittaa CSV-tiedostoja käyttäen `CSVReader` ja `CSVWriter` luokkia.

Voimme myös käyttää `@CsvBindByName` annotaatiota luokan ominaisuuksien määrittämiseen, jolloin `opencsv` kirjasto hoitaa automaattisesti tietojen käsittelyn.

## Katso myös

- [Java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Kotlinx.io](https://kotlinlang.org/api/latest/kotlinx.io/kotlin.io/)
- [OpenCSV 5.3](http://opencsv.sourceforge.net/)