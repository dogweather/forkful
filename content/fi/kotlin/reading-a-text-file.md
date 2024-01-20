---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Luemme tekstitiedostoja ohjelmassa, jotta voimme käsitellä tiedoston sisältämää tietoa tehtävämme mukaan. Tämä on välttämätöntä datan käsittelyssä, esimerkiksi koneoppimisessa tai datatutkimuksissa.

## Miten näin:

Kotlin tarjoaa natiivin tuen tiedostojen lukemiselle `readText()`-funktion avulla. Katso esimerkki alla:

```kotlin
import java.io.File

fun main() {
    val content = File("example.txt").readText()
    println(content)
}
```
Jos tiedostossamme `example.txt` on teksti "Hei Suomi!", ohjelma tulostaa:

```
Hei Suomi!
```

## Syvällisemmin: 

Historiallisesti Java-koodia käytettiin tiedostojen käsittelyyn, mutta Kotlin tarjoaa paljon puhtaamman ja seän tiiviimmän syntaksin. Voit myös valita lukemisen riveittäin `readLines()`-funktiolla:

```kotlin
import java.io.File

fun main() {
    val lines = File("example.txt").readLines()
    lines.forEach { line -> println(line) }
}
```

Jos tiedostomme sisältää useita rivejä, jokainen rivi tulostetaan omalle rivilleen.

Vaihtoehtoisesti, voit käyttää `BufferedReader`-luokkaa suurten tiedostojen lukemiseen, jotta vältätäisimme muistiongelmat:
```kotlin
val bufferedReader: BufferedReader = File("largefile.txt").bufferedReader()
```

## Katso myös: 

Kotlinin dokumentoinnista löytyy lisää tietoa tiedostojen käsittelystä tässä linkissä: [Kotlin File Reading](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html). Jos haluat lukea suurista tiedostoista optimoidusti, kannattaa tutustua tämän linkin opastukseen: [Reading Large Files Efficiently](https://www.baeldung.com/java-reading-large-files).