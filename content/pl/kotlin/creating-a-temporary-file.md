---
title:                "Tworzenie pliku tymczasowego."
html_title:           "Kotlin: Tworzenie pliku tymczasowego."
simple_title:         "Tworzenie pliku tymczasowego."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Tworzenie tymczasowych plików jest powszechną praktyką w programowaniu. Polega to na tworzeniu plików, które są potrzebne tylko na krótki czas i nie są przechowywane trwale. Programiści często tworzą tymczasowy plik, aby tymczasowo przechowywać dane lub wykonywać pewne operacje.

## Jak to zrobić:
```Kotlin
import java.io.File

fun main() {
    val tempFile = createTempFile() //tworzenie nowego tymczasowego pliku
    tempFile.writeText("To jest wygenerowany tymczasowy plik.") //zapisanie tekstu w pliku
    println(tempFile.readText()) //wypisanie zawartości pliku na konsolę
    tempFile.delete() //usunięcie tymczasowego pliku
}
```
Wyjście:
```
To jest wygenerowany tymczasowy plik.
```

## Głębszy wgląd:
Tworzenie tymczasowych plików jest szczególnie przydatne, kiedy potrzebujemy przetworzyć duży plik lub gdy chcemy zapisać tymczasowe dane. Jest to również użyteczne w testach jednostkowych, gdy potrzebujemy tymczasowego pliku do przetestowania kodu. Alternatywą dla tworzenia tymczasowych plików jest przechowywanie danych w pamięci operacyjnej, jednak w przypadku dużych danych, może to skutkować brakiem pamięci. W języku Kotlin funkcja createTempFile() jest dostępna dzięki bibliotece Java, która dostarcza szereg narzędzi do zarządzania plikami.

## Zobacz także:
- [Dokumentacja języka Kotlin - createTempFile()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Stosowanie tymczasowych plików w programowaniu](https://www.baeldung.com/java-create-temporary-file)