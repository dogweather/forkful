---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapisywanie pliku tekstowego to proces tworzenia lub modyfikowania plików zawierających tekstowe dane. Programiści wykonują tę czynność do trwałego zapisu danych, konfiguracji, logów aplikacji czy wymiany informacji.

## Jak to zrobić:
```kotlin
import java.io.File

fun main() {
    val tekstDoZapisu = "Cześć, to jest przykładowy tekst."
    File("przykladowy_plik.txt").writeText(tekstDoZapisu)
    println("Tekst zapisany do pliku: przykladowy_plik.txt")
}
```
Wynik na konsoli:
```
Tekst zapisany do pliku: przykladowy_plik.txt
```

## Więcej szczegółów:
Zapis pliku tekstowego jest możliwy w Kotlinie dzięki klasom z Javy, jak `File`. Alternatywnie, można użyć `PrintWriter` czy `FileOutputStream` dla większych możliwości, np. buforowania danych. Zapisywanie plików ewoluowało od prostych poleceń w językach takich jak BASIC, przez C czy Java, do bardziej wygodnych metod w Kotline.

## Zobacz również:
- [Dokumentacja klasy File w Kotlinie](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Poradnik do I/O w Kotlinie (ang.)](https://www.baeldung.com/kotlin/read-file)
