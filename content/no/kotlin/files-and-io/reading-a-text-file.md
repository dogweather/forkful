---
date: 2024-01-20 17:54:34.879289-07:00
description: "\xC5 lese en tekstfil betyr at vi henter innholdet i fila til programmet\
  \ v\xE5rt. Programmerere gj\xF8r dette for \xE5 behandle data, konfigurere systemer\
  \ eller laste\u2026"
lastmod: '2024-03-13T22:44:40.769020-06:00'
model: gpt-4-1106-preview
summary: "\xC5 lese en tekstfil betyr at vi henter innholdet i fila til programmet\
  \ v\xE5rt."
title: Lese en tekstfil
weight: 22
---

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
