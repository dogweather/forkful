---
title:                "Lese en tekstfil"
aliases:
- no/kotlin/reading-a-text-file.md
date:                  2024-01-20T17:54:34.879289-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil betyr at vi henter innholdet i fila til programmet vårt. Programmerere gjør dette for å behandle data, konfigurere systemer eller laste inn ressurser.

## Hvordan:
```kotlin
import java.io.File

fun main() {
    val content = File("example.txt").readText()
    println(content)
}
```

**Utskrift:**
```
Dette er innholdet i tekstfila.
```

Eller for å lese linje for linje:

```kotlin
import java.io.File

fun main() {
    File("example.txt").forEachLine { line ->
        println(line)
    }
}
```

**Utskrift:**
```
Første linje i fila.
Andre linje i fila.
```

## Dykk Ned
Før Kotlin og moderne språk, som Python eller Ruby, gjorde filoperasjoner enkle, hadde Java og C programmerere en mer komplisert oppgave. Med `java.io.*` måtte du håndtere `InputStreams`, `Readers`, og unntak for feilhåndtering.

Alternativer for å lese tekstfiler innebærer bruk av `BufferedReader` for bedre ytelse med store filer, eller `Scanner` for å parse primitive typer og strenger med regulære uttrykk.

I Kotlin er det viktig å vite at `readText()` laster hele filinnholdet i minnet, så det er ikke ideelt for store filer. `forEachLine` er et bedre valg da det bearbeider én linje av gangen.

## Se Også
- [Kotlin documentation on reading files](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [JetBrains discussion on file operations](https://discuss.kotlinlang.org/t/working-with-files-in-kotlin/2368)
- [Baeldung Kotlin file reading](https://www.baeldung.com/kotlin/read-file)
