---
title:    "Kotlin: Tilapäistiedoston luominen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi luoda tilapäistiedosto?

Tilapäistiedostojen luominen on hyödyllistä, kun ohjelma tarvitsee tallentaa tilapäistä tietoa, jota ei tarvita ohjelman suorituksen lopuksi. Tämä vähentää pysyvien tiedostojen määrää, mikä voi auttaa säästämään tilaa ja pitämään tiedostojärjestelmän siistimpänä.

## Miten luoda tilapäistiedosto Kotlinissa

Tilapäistiedoston luominen Kotlinissa on helppoa käyttämällä Javan NIO (New Input/Output) -pakettia. Alla olevassa koodiesimerkissä luodaan tilapäistiedosto nimeltä "tilapäistiedosto.txt" ja kirjoitetaan siihen "Tervetuloa!".

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption

fun main() {
    val file = Files.createTempFile("tilapäistiedosto", ".txt")
    Files.write(file, "Tervetuloa!".toByteArray(), StandardOpenOption.APPEND)
    println("Tilapäistiedosto luotu: " + file.toAbsolutePath())
}
```
Tässä tapauksessa, tulosteena on:
```
Tilapäistiedosto luotu: C:\Users\Käyttäjä\AppData\Local\Temp\tilapäistiedosto202019-05-05143251.txt
```
Tilapäistiedoston nimi muodostuu annetusta nimestä, päiväyksestä ja kellonajasta.

## Syvempi sukellus tilapäistiedostojen luomiseen

Tilapäistiedostot näyttävät tavallisilta tiedostoilta, mutta niillä on kaksi tärkeää eroa: ne poistetaan automaattisesti ohjelman suorituksen jälkeen ja niiden polku vaihtelee riippuen käyttöjärjestelmästä. Tilapäistiedostoja voi myös luoda omilla nimillä ja poluilla käyttämällä `createTempFile` -funktiota ja antamalla polun ja nimen parametreiksi.

See Also
- [Kotlin: Javan NIO](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.nio.-package.html)
- [Java: Luodaan tilapäissä tiedostoja](https://www.javacodegeeks.com/2013/04/java-create-temporary-file-in-system-temp-directory.html)