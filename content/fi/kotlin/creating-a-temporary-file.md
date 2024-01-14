---
title:                "Kotlin: Väliaikaisen tiedoston luominen"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda väliaikainen tiedosto?

Kotlinin avulla voit luoda väliaikaisia tiedostoja, jotka ovat tarpeen monissa eri tilanteissa ohjelmoinnissa. Näihin tilanteisiin voi kuulua esimerkiksi tiedostojen tallentaminen ennen niiden lopullista tallentamista tai testitapausten luominen, joissa tarvitaan väliaikaista tiedostoa.

## Kuinka luoda väliaikainen tiedosto Kotlinilla?

Kotlinissa on käytettävissä `createTempFile()` -funktio, jonka avulla voidaan luoda väliaikainen tiedosto. Funktiolle voidaan antaa parametreina muun muassa haluttu tiedostonimi ja tiedostopäätteen tyyppi.

```Kotlin
val temporaryFile = createTempFile("tiedostonimi", ".txt")
println("Luotu väliaikainen tiedosto: $temporaryFile")
```

Tämä koodi luo väliaikaisen tiedoston nimeltä "tiedostonimi.txt" ja tulostaa tiedoston tiedon konsoliin.

## Syvemmälle väliaikaisen tiedoston luomiseen

Kotlinin `createTempFile()` -funktio toimii luomalla tiedoston oletuspaikkaan, joka on käyttöjärjestelmän määrittämä väliaikaisten tiedostojen tallennuspaikka. Tähän tallennuspaikkaan voi päästä käsiksi `tmpdir` -muuttujan avulla.

Lisäksi `createTempFile()` -funktio palauttaa luotujen tiedostojen käsittelyyn tarkoitetun `File` -luokan instanssin, jonka kautta voidaan tehdä erilaisia toimenpiteitä tiedoston kanssa.

```Kotlin
val temporaryFile = createTempFile("tiedostonimi", null, File("oma/polku/"))
println("Luotu väliaikainen tiedosto: $temporaryFile")
println("Tiedoston koko: ${temporaryFile.length()} tavua")
```

Ylläoleva koodiesimerkki luo väliaikaisen tiedoston omalle polulle ja tulostaa tiedoston koon konsoliin. Tiedoston koon voi tarkistaa esimerkiksi validointitarkoituksia varten.

## Katso myös

- [Kotlinin `createTempFile()`-funktion dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Väliaikaisen tiedoston luominen Javan kanssa](https://www.baeldung.com/java-temporary-file)