---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns är processen att bestämma om en viss katalog existerar inom ditt filsystem. Programmers gör detta för att undvika fel som kan uppstå vid försök att manipulera en katalog som inte existerar.

## Hur man gör:
Här är hur du kan kontrollera om en katalog finns med Kotlin:

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/home/myDirectory")

    if (Files.exists(path)) {
        println("Katalogen finns.")
    } else {
        println("Katalogen finns inte.")
    }
}
```

Om katalogen finns kommer detta att skriva ut "Katalogen finns.". Annars kommer det att skriva ut "Katalogen finns inte.".

## Djupdykning:
1. **Historisk kontext:** Kotlin är ett relativt ungt programmeringsspråk som introducerades 2011 av JetBrains. Trots det har det snabbt blivit mycket populärt, särskilt för Android-utveckling, tack vare sin stramlade syntax och moderna funktioner. Förmågan att kontrollera om en katalog finns är bara en av dessa många praktiska funktioner.

2. **Alternativ:** Förutom detta `Files.exists(path)`-metod, kan du också använda klassiska Java I/O metoder för att kontrollera om en fil eller katalog existerar. Men Kotlin's `Files.exist`-metod är mer läsbar och rekommenderad att använda.

3. **Implementationsdetaljer:** `Files.exists(path)`-metoden kontrollerar faktiskt om den angivna sökvägen existerar och är nåbar på filsystemet.

## Se också:
Kotlin officiella dokumentation: https://kotlinlang.org/docs/reference/
Using Paths and Files in Java: https://docs.oracle.com/javase/tutorial/essential/io/pathOps.html