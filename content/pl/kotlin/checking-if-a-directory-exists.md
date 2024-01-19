---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Kotlin: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Sprawdzanie, czy katalog istnieje, to proces potwierdzania obecności określonego katalogu w systemie plików. Programiści robią to, aby zapobiec błędom wykonania podczas operacji plikowych.

## Jak to zrobić:

Skorzystaj z metody `exists()` klasy `java.nio.file.Files` do sprawdzenia, czy dany katalog istnieje. Oto przykład:

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/sciezka/do/katalogu")
    if (Files.exists(path)) {
        println("Katalog istnieje")
    } else {
        println("Katalog nie istnieje")
    }
}
```

## Głębsza wiedza:

Kiedy Kotlin został po raz pierwszy zaprezentowany w 2011 roku, przyjęcia znacznej ilości funkcji z Javy nie było jeszcze pewne. Ale Trust JetBrains, twórcy Kotlin, dostrzega świetność Javy i wiąże Kotlin bardzo blisko z Javą, szczególnie jeśli chodzi o zarządzanie plikami i katalogami.

Alternatywnie, możemy użyć klasy `java.io.File`. Ale `java.nio.file.Files` jest lepszym rozwiązaniem, ponieważ jest to nowoczesne podejście do obsługi plików w Javie.

Szczególnie ważne jest sprawdzanie istnienia katalogu w przypadku operacji zapisu. Bez sprawdzania, nasz program może próbować utworzyć plik w miejscu, które nie istnieje, co skutkuje błędem czasu wykonania.

## Zobacz również:

- Dokumentacja Kotlin: https://kotlinlang.org/docs/reference/
- Dokumentacja Java NIO: https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html
- Dokumentacja Java IO: https://docs.oracle.com/javase/8/docs/api/java/io/File.html