---
title:                "Kotlin: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda väliaikainen tiedosto?

On monia tilanteita, joissa haluat luoda väliaikaisen tiedoston ohjelman suorituksen aikana. Väliaikaisia tiedostoja voi tarvita esimerkiksi tiedon tallentamiseen, jakamiseen tai prosessointiin, mutta niitä ei välttämättä tarvita pysyvästi. Tämä voi auttaa välttämään tietojen sekoittumista tai tarpeettomien tiedostojen pysyvää tallentamista.

## Kuinka luoda väliaikainen tiedosto

Voit luoda väliaikaisen tiedoston helposti käyttämällä Kotlinin `createTempFile()` -funktiota. Se ottaa kaksi parametria: tiedoston nimen ja tiedostopäätteen. Funktio luo väliaikaisen tiedoston automaattisesti järjestelmän tmp-hakemistoon ja palauttaa sen `File` -objektina.

```Kotlin
val tempFile = File.createTempFile("temp", ".txt")
println(tempFile.name) // tulostaa "temp1561574522292066844.txt"
```

Voit myös määrittää tarvittaessa tiedoston tallennuspaikan:

```Kotlin
val tempFile = File.createTempFile("temp", ".txt", File("C:/temp"))
println(tempFile.path) // tulostaa "C:/temp/temp1561574522292066844.txt"
```

## Syvempi sukellus väliaikaisen tiedoston luomiseen

Käyttäessäsi `createTempFile()` -funktiota, sinun kannattaa huomioida muutamia asioita:

- Jos et määrittele tiedoston tallennuspaikkaa, järjestelmän tmp-hakemisto käytetään automaattisesti. Tiedostot tallennetaan usein oletuksena jonnekin käyttöjärjestelmän määrittelemään tmp-hakemistoon.
- Väliaikaiset tiedostot ovat oletuksena poistettavia, eli niitä ei tallenneta pysyvästi. Jos haluat tallentaa tiedoston pysyvästi, voit käyttää `deleteOnExit()` -funktiota.
- Varmista, että käytät uniikkia tiedostopäätettä, jotta vältytään mahdollisilta yhteentörmäyksiltä järjestelmän jo olemassa olevien tiedostojen kanssa.

## Katso myös

- [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html) väliaikaisen tiedoston luomisesta
- [Väliaikaiset tiedostot Windows-käyttöjärjestelmässä](https://ss64.com/nt/syntax-temp.html)