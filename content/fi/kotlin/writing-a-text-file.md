---
title:    "Kotlin: Tekstitiedoston kirjoittaminen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on yksi tärkeimmistä taidoista ohjelmoinnissa, ja tekstitiedostojen kirjoittaminen on keskeinen osa sitä. Tekstitiedostojen kirjoittamisen avulla voit tallentaa ja säilyttää tietoja eri ohjelmien välillä.

## Miten

Kotlin-ohjelmointikieli tarjoaa helpon tavan kirjoittaa tekstitiedostoja. Se käyttää Java IO-kirjastoa, joka tarjoaa joukon luokkia ja metodeja tiedostojen käsittelyyn.

```
Kotlin
val tiedosto: File = File("tietoja.txt") // Luo uuden tiedoston
tiedosto.writeText("Hei, tämä on esimerkki tekstiä.") // Kirjoittaa tiedostoon tekstin
println(tiedosto.readText()) // Tulostaa tiedostoon tallennetun tekstin
```

Tässä esimerkissä luomme ensin uuden tiedoston nimeltä "tietoja.txt". Sitten käytämme `writeText()`-metodia kirjoittamaan tiedostoon otsikon "Hei, tämä on esimerkki tekstiä". Lopuksi tulostamme tiedostosta luetun tekstin käyttäen `readText()`-metodia.

```
Kotlin
Hei, tämä on esimerkki tekstiä.
```

## Syventävä tarkastelu

Kun kirjoitat tekstitiedostoja Kotlinilla, on hyvä myös huolehtia siitä, että tiedostot suljetaan ja virheenkäsittely tapahtuu asianmukaisesti. Tämä voi vaikuttaa ohjelman suorituskykyyn ja varmistaa, että tiedostoista ei tule jumiutuneita.

Lisäksi on hyödyllistä käyttää `FileWriter`-luokkaa, joka tarjoaa enemmän vaihtoehtoja tiedoston kirjoittamiseen, kuten lisätä tiedostoon sisältöä sen sijaan, että korvaisi sen kokonaan.

```
Kotlin
val tiedosto: File = File("lisays.txt")
val lisays: String = "Lisää tekstiä tiedostoon."
val fileWriter: FileWriter = FileWriter(tiedosto, true) // Toinen parametri tarkoittaa, että sisältöä lisätään eikä korvata
fileWriter.use { it.write(lisays) } // "use" metodi sulkee tiedoston automaattisesti
```

## Katso myös

- [Kotlin ohjelmointikielen viralliset sivut](https://kotlinlang.org/docs/jvm-get-started.html)
- [Java IO-kirjaston dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html)
- [Kotin IO-kirjaston dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)