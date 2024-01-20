---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie pliku tekstowego to proces odczytywania danych zapisanych w formacie pliku tekstowego (.txt, .log, .xml itp.) przez program. Programiści robią to, aby na przykład przetwarzać logi, odczytywać dane konfiguracyjne, zmieniać pliki XML i wiele innych zastosowań.

## Jak to zrobić:

Przykładowy kod w Kotlinie do odczytu pliku tekstowego wygląda tak:

```Kotlin
import java.io.File

fun main(args: Array<String>) {
    val fileName = "test.txt"

    val lines: List<String> = File(fileName).readLines()

    lines.forEach { line -> println(line) }
}
```
Jeżeli nasz plik `test.txt` zawierałby np. takie linie:

```
Pierwsza linia
Druga linia
Trzecia linia
```

To nasz program wydrukowałby na ekranie:
```
Pierwsza linia
Druga linia
Trzecia linia
```

## W głąb tematu:

Kotlin, jak wiele innych języków programowania, posiada wbudowane funkcje do manipulowania plikami tekstowymi. Tym niemniej, warto znać też alternatywne sposoby jak na przykład biblioteki zewnętrzne np. Apache Commons IO. Ważnym aspektem jest również zrozumienie, jak odczyt plików jest implementowany na poziomie systemu operacyjnego - Back in the days, operacje na plikach były dużo bardziej skomplikowane, ale na szczęście teraz wielu z tych szczegółów jest ukrytych za prostymi API.

## Zobacz też:

- Dokumentacja Kotlin do odczytu plików tekstowych: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-lines.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-lines.html)
- Apache Commons IO, zewnętrzna biblioteka do obsługi plików: [https://commons.apache.org/proper/commons-io/](https://commons.apache.org/proper/commons-io/)
- Artykuł na temat odczytu plików w Kotlinie: [https://www.baeldung.com/kotlin-read-file](https://www.baeldung.com/kotlin-read-file)