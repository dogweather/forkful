---
title:                "Odczytywanie pliku tekstowego"
aliases:
- /pl/kotlin/reading-a-text-file/
date:                  2024-01-20T17:54:57.380621-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Czytanie pliku tekstowego to pobieranie danych z pliku zapisanego na dysku. Programiści to robią, aby wczytać konfiguracje, dane wejściowe lub po prostu tekst do przetworzenia.

## How to: (Jak to zrobić:)
```kotlin
import java.io.File

fun main() {
    val textContent = File("example.txt").readText(Charsets.UTF_8)
    println(textContent)
}
```
Sample output (Przykładowy wynik):
```
To jest treść pliku tekstowego.
```

Aby czytać plik linia po linii:
```kotlin
import java.io.File

fun main() {
    File("example.txt").forEachLine { line ->
        println(line)
    }
}
```

## Deep Dive (Dogłębna analiza)
Czytanie plików tekstowych jest podstawowym elementem programowania od jego początków. W Kotlinie, operacje na plikach są ułatwione przez standardową bibliotekę, która zapewnia metody takie jak `readText` i `forEachLine`. 

Alternatywie można użyć `BufferedReader` dla większej efektywności przy dużych plikach:
```kotlin
File("example.txt").bufferedReader().use { reader ->
    var line: String?
    while (reader.readLine().also { line = it } != null) {
        println(line)
    }
}
```

Zaawansowane implementacje mogą uwzględniać obsługę błędów i kodowanie znaków. Pamiętaj, że obsługa błędów jest kluczowa, gdy pliki mogą nie istnieć lub nie mieć odpowiednich uprawnień do czytania.

## See Also (Zobacz również)
- Dokumentacja Kotlin na temat pracy z IO: [Kotlin IO](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- Wskazówki na temat obsługi błędów IO w Kotlinie: [Kotlin Exception Handling](https://kotlinlang.org/docs/exceptions.html)
