---
title:                "Töiden tekeminen csv:n kanssa"
html_title:           "Kotlin: Töiden tekeminen csv:n kanssa"
simple_title:         "Töiden tekeminen csv:n kanssa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

Monet yritykset ja organisaatiot käyttävät CSV-muotoa tallentaakseen ja jakaa tietoa. Näiden tiedostojen käsittely on tärkeä taito, joka voi parantaa tuottavuutta ja tehokkuutta.

## Miten

```Kotlin
import java.io.File
import kotlin.io.readLine

// Luodaan uusi CSV-tiedosto
val tiedosto = File("tiedosto.csv")

// Kirjoitetaan rivi tiedostoon
tiedosto.appendText("Nimi, Ikä, Sukupuoli\n")

// Luetaan tiedoston sisältö ja tallennetaan se muuttujaan
val sisalto = tiedosto.readLines()

// Käytetään sisältöä
println(sisalto[0]) // tulostaa "Nimi, Ikä, Sukupuoli"

// Muutetaan sisältö listaan
val tietueet = sisalto.map{it.split(",")}
// Tulostetaan toisen tietueen ikä
println(tietueet[1][1]) // tulostaa "25"
```

## Syväsukellus

CSV-tiedosto koostuu riveistä ja sarakkeista, joita erottaa erityinen merkki, usein pilkku. Kotlin tarjoaa monia tehokkaita toimintoja, kuten `readLines()` ja `split()`, jotka helpottavat tiedoston käsittelyä. On myös mahdollista käyttää erilaisia ​​kirjastoja ja työkaluja, kuten *Apache Commons CSV*, jotka tarjoavat enemmän toiminnallisuuksia.

## Katso myös

- [Java, CSV ja Apache Commons CSV](https://www.baeldung.com/apache-commons-csv)
- [CSV-tiedostojen käsittely Kotlinilla](https://www.callicoder.com/kotlin-convert-file-csv-to-arraylist-write-csv-file/)