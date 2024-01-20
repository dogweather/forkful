---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego? 
Tworzenie tymczasowych plików to proces, w którym tworzysz plik do przechowywania danych na krótki czas. Programiści robią to zazwyczaj, kiedy chcą przechować informacje między operacjami, ale nie chcą zanieczyszczać stałym plikiem.

## Jak to zrobić:
Oto, jak w Kotlinie możemy stworzyć plik tymczasowy i zapisać do niego dane.
```Kotlin
import java.nio.file.Files
import java.nio.file.Path

fun main() {
    val tempFile: Path = Files.createTempFile("temp", ".txt")
    println("Tymczasowy plik utworzony w lokalizacji: ${tempFile.toAbsolutePath()}")

    val data = "Tymczasowa zawartość pliku"
    Files.write(tempFile, data.toByteArray())
    println("Zawartość zapisana do pliku tymczasowego")
}
```
Podczas uruchamiania powyższego kodu, zobaczysz coś na wzór:
```
Tymczasowy plik utworzony w lokalizacji: /tmp/temp123456789.txt
Zawartość zapisana do pliku tymczasowego
```

## Przyjrzyjmy się temu bliżej
Tworzenie tymczasowych plików ma swoje korzenie we wczesnych dniach programowania, gdzie zasoby były ograniczone. Do dziś jest to przydatne narzędzie dla programistów, szczególnie do optymalizacji pamięci i przetwarzania danych.

Alternatywą dla tworzenia plików tymczasowych jest użycie bazy danych lub zapamiętanie danych w pamięci. Wybór zależy od specyfiki danego zadania. 

Jedną z kluczowych kwestii związanych z tworzeniem plików tymczasowych jest to, że są usuwane po zakończeniu programu. Możemy to zrobić za pomocą metody `deleteOnExit()` na obiekcie `File`.

## Zobacz też

1. [Dokumentacja Kotlin - Tworzenie plików](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
2. [Java NIO.2 File API w Kotlinie](https://www.baeldung.com/kotlin/java-nio2-file-api)

Pamiętaj, że tworzenie plików tymczasowych to potężne narzędzie, ale jej używanie powinno być dobrze przemyślane. Zawsze zastanów się nad zaletami i wadami takiego podejścia.