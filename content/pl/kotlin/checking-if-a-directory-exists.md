---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:57:26.506770-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Sprawdzanie, czy katalog istnieje w systemie plików, to kontrola przed próbą dostępu czy modyfikacji. Programiści robią to, aby uniknąć błędów i zapewnić prawidłowe działanie aplikacji.

## How to: (Jak to zrobić:)
Użyjemy `File` z Javy w Kotlinie:

```kotlin
import java.io.File

fun main() {
    val directoryPath = "/path/to/directory"
    val directory = File(directoryPath)

    if (directory.exists() && directory.isDirectory) {
        println("Katalog istnieje: $directoryPath")
    } else {
        println("Katalog nie istnieje.")
    }
}
```
Jeśli katalog istnieje, output to:
```
Katalog istnieje: /path/to/directory
```
Jeśli nie, output to:
```
Katalog nie istnieje.
```

## Deep Dive (Dogłębna analiza):
Sprawdzanie czy katalog istnieje nie jest nowością – funkcje do tego służące są w językach programowania od dawna. W Kotlinie korzystamy bezpośrednio z klas Javy, bo Kotlin jest z nią kompatybilny. Alternatywy? Możesz użyć `Files.exists(Paths.get(directoryPath))` z Java NIO dla większej elastyczności i czytelności kodu. Gdy sprawdzasz istnienie katalogu, pamiętaj o uprawnieniach – możesz napotkać `SecurityException`. Optymalizacja? Sprawdzaj istnienie tylko wtedy, gdy to konieczne – nadmiarowe sprawdzanie może spowolnić aplikację.

## See Also (Zobacz również):
- [Kotlin Documentation](https://kotlinlang.org/docs/reference/)
- [Java File Class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java NIO Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Handling Permissions in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)