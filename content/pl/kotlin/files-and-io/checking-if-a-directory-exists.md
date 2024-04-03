---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:42.416439-07:00
description: "Jak to zrobi\u0107: Kotlin, dzia\u0142aj\u0105cy na JVM, wykorzystuje\
  \ Java File API do operacji na plikach, co czyni sprawdzanie istnienia katalog\xF3\
  w prostym. Oto\u2026"
lastmod: '2024-03-13T22:44:35.379603-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, dzia\u0142aj\u0105cy na JVM, wykorzystuje Java File API do operacji\
  \ na plikach, co czyni sprawdzanie istnienia katalog\xF3w prostym."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
Kotlin, działający na JVM, wykorzystuje Java File API do operacji na plikach, co czyni sprawdzanie istnienia katalogów prostym. Oto podstawowy przykład:

```kotlin
import java.io.File

fun main() {
    val path = "/ścieżka/do/katalogu"
    val katalog = File(path)

    if (katalog.exists() && katalog.isDirectory) {
        println("Katalog istnieje: $path")
    } else {
        println("Katalog nie istnieje: $path")
    }
}
```
Przykładowe wyjście, zakładając, że katalog istnieje:
```
Katalog istnieje: /ścieżka/do/katalogu
```
A jeśli nie:
```
Katalog nie istnieje: /ścieżka/do/katalogu
```

W projekcie Kotlin, możesz również często pracować z bibliotekami lub frameworkami specyficznymi dla Kotlina, jak Ktor do aplikacji webowych czy kotlinx.coroutines do programowania asynchronicznego. Jednakże, do sprawdzania, czy katalog istnieje, standardowe Java `File` API, jak pokazano, jest zazwyczaj wystarczające i szeroko stosowane ze względu na interoperacyjność Kotlina z Javą. Nie są wymagane żadne biblioteki stron trzecich do tego konkretnego zadania, co czyni je dostępnym i prostym dla początkujących przechodzących z innych języków programowania na Kotlina.
