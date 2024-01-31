---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:57:33.668246-07:00
simple_title:         "Kontrollera om en katalog finns"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kontrollera om en katalog (directory) finns handlar om att se till att den plats där din app eller program ska lagra eller hitta data verkligen finns tillgänglig. Programmerare gör detta för att undvika fel vid filåtkomst och för att säkerställa att programmet kan hantera filer korrekt.

## Hur man gör:

Här är ett enkelt sätt att kolla om en katalog finns i Kotlin:

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun doesDirectoryExist(path: String): Boolean {
    return Files.exists(Paths.get(path))
}

fun main() {
    val directoryPath = "/path/to/directory"
    if (doesDirectoryExist(directoryPath)) {
        println("Katalogen finns!")
    } else {
        println("Katalogen finns inte.")
    }
}
```

Kör programmet och få output beroende på om katalogen finns eller inte:

```
Katalogen finns!
```

eller

```
Katalogen finns inte.
```

## Fördjupning

Kontroll av katalog finns från de tidiga dagarna av programmering. Alternativ till `Files.exists` i Kotlin kan inkludera att använda `File` klassen från Java:

```Kotlin
import java.io.File

fun doesDirectoryExistUsingFile(path: String): Boolean {
    return File(path).exists()
}
```

Det är betydelsefullt att notera att `Files.exists` och `File.exists` kan ge olika resultat under vissa omständigheter. `Files.exists` räknar till exempel inte tillgängliga men ej läsbara kataloger som "existerande", medan `File.exists` gör det.

## Se även

- Officiell Kotlin-dokumentation om filhantering: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- Java `Files` klassdokumentation: [https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- Java `File` klassdokumentation: [https://docs.oracle.com/javase/8/docs/api/java/io/File.html](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
