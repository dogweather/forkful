---
title:                "Luettaessa tekstitiedostoa"
html_title:           "Kotlin: Luettaessa tekstitiedostoa"
simple_title:         "Luettaessa tekstitiedostoa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tekstikansion lukeminen on yksi ohjelmoinnin perustaidoista. Se tarkoittaa tiedostojen lukemista ja käsittelemistä, jotka sisältävät tekstiä, kuten sanoja, lauseita ja numeroita. Ohjelmoijat käyttävät tätä taitoa päästäkseen käsiksi tietoon ja käsitelläkseen sitä.

## Kuinka tehdä se:
Kotlinilla, voit lukea tiedostoja käyttäen "FileReader" ja "BufferedReader" luokkia. Näiden avulla voit avata tiedoston ja lukea sen sisältöä rivi riviltä. Tässä on esimerkki koodista:

```Kotlin
import java.io.FileReader
import java.io.BufferedReader

fun main() {

    // Avaa tiedosto lukemista varten
    val reader = BufferedReader(FileReader("tiedosto.txt"))

    // Luo muuttuja, johon tallennetaan tiedoston sisältö
    var sisalto: String = ""

    // Luetaan tiedosto rivi riviltä
    var rivi = reader.readLine()

    while (rivi != null) {

        // Lisätään rivi sisältöön
        sisalto += rivi

        // Luetaan seuraava rivi
        rivi = reader.readLine()
    }

    // Tulostetaan tiedoston sisältö
    println(sisalto)

    // Suljetaan tiedoston lukeminen
    reader.close()
}
```

Tämän esimerkin avulla voit lukea ja tulostaa tiedoston sisällön. Voit myös käsitellä tiedoston sisältöä haluamallasi tavalla, esimerkiksi etsimällä tietyt sanat tai numerot.

## Syvemmälle:
Tiedostojen lukeminen on ollut osa ohjelmointia aina alusta asti. Ennen kuin oli olemassa moderneja tietokoneita, ohjelmoijat käyttivät nappeja ja lappuja lukeakseen ja tallentaakseen tietoa.

Tiedostojen lukemisen lisäksi on myös muita tapoja käsitellä ja päästä käsiksi tietoon, kuten käyttämällä tietokantaa. Tämä voi olla parempi vaihtoehto, jos sinun tarvitsee tallentaa ja käsitellä suuria määriä tietoa.

Tiedostojen lukemisen toteutus riippuu käyttämästäsi ohjelmointikielestä ja käyttöympäristöstä. Kotlinilla, voit käyttää myös "Scanner" luokkaa tiedostojen lukemiseen.

## Katso myös:
Voit lukea lisää tiedostojen käsittelystä Kotlinilla [Kotlinin virallisesta dokumentaatiosta](https://kotlinlang.org/docs/reference/reading-writing-files.html). Voit myös tutustua muihin tapoihin käsitellä tiedostoja ja tietoa, kuten tietokantoihin ja REST API:in, joka tarjoaa mahdollisuuden käsitellä ja jakaa tietoa verkon kautta.