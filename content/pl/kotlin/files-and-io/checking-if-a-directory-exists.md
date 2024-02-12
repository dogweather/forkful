---
title:                "Sprawdzanie, czy katalog istnieje"
aliases:
- /pl/kotlin/checking-if-a-directory-exists/
date:                  2024-02-03T19:07:42.416439-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje w Kotlinie, polega na weryfikacji obecności katalogu pod określoną ścieżką. Programiści wykonują to zadanie, aby zapobiec błędom, takim jak próba odczytu z katalogu, który nie istnieje, lub zapisu do niego, zapewniając płynniejszą obsługę plików i zarządzanie danymi w aplikacjach.

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
