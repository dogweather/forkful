---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Kotlin: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tarkastellaan toimintoa, jolla tarkistetaan, onko hakemisto olemassa. Sen käyttäjät pystyvät välttämään virheitä, jotka johtuvat siitä, että he yrittävät käsitellä olemattomia hakemistoja.

## Kuinka seuraavasti:
Alla näet esimerkin siitä, miten Kotlinissa voit tarkistaa, onko kansio olemassa.

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/path/to/directory")

    val exists = Files.exists(path)

    println("Directory exists: $exists")
}
```

Syötä komento ja saat tuloksen:

```Instance
Directory exists: true
```
tai

```Instance
Directory exists: false
```

## Syvempi sukellus
Historiallisesti Java-versioissa ennen Javan NIO.2: ta (joka esiteltiin Javassa 7) oli vaikea tehdä yksinkertaisia ​​tiedostojärjestelmätoimintoja, kuten tarkistaa, onko kansio olemassa. Kotlin hyödyntää Javassa 7 esiteltyjä NIO.2-toimintoja parantaakseen tätä kokemusta.

Vaihtoehtoinen tapa tarkistaa onko kansio olemassa on `File` luokka. Esimerkki:

```Kotlin
import java.io.File

fun main() {
    val directory = File("/path/to/directory")

    val exists = directory.exists()

    println("Directory exists: $exists")
}
```

Vaikka tämä menetelmä on helpompi aloittelijoille ymmärtää, se ei välttämättä ole yhtä joustava kuin `Files.exists` -menetelmä.
  
## Katso myös
1. [Oracle Java Docs: Checking a File or Directory](https://docs.oracle.com/javase/tutorial/essential/io/check.html)
2. [Kotlin Docs: java.io.File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
3. [Baeldung: How to Check if a Directory Exists in Java](https://www.baeldung.com/java-check-directory-exists)