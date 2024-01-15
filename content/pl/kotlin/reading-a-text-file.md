---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Kotlin: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Prawdopodobnie zastanawiasz się, po co ci czytanie plików tekstowych w Kotlinie. Cóż, jest to częsta czynność w programowaniu, zwłaszcza jeśli pracujesz z danymi. Dzięki temu artykułowi dowiesz się, jak w łatwy sposób czytać pliki tekstowe i wykorzystywać te informacje do swoich celów.

## Jak to zrobić

Aby czytać pliki tekstowe w Kotlinie, potrzebne będą nam dwa elementy: strumienie (streams) oraz wykorzystanie funkcji readFile(). Spójrzmy na przykład kodu, który pokaże Ci, jak dokładnie to zrobić:

```
import java.io.File

fun main() {
    val file = File("sample.txt")
    val lines = file.readLines()
    for (line in lines) {
        println(line)
    }
}
```

Wykorzystaliśmy klasę File, aby odwołać się do pliku "sample.txt" w naszym projekcie. Następnie za pomocą funkcji readLines() odczytaliśmy zawartość pliku i przypisaliśmy ją do zmiennej lines. W pętli for wyświetliliśmy każdą linię pliku na konsoli. Proste, prawda? Aby wykorzystać te informacje w dalszej części naszego kodu, możemy np. przypisać je do innych zmiennych lub wykorzystać w wyrażeniach warunkowych.

## Deep Dive

Jeśli chcesz zagłębić się w temat czytania plików tekstowych w Kotlinie, warto przeczytać oficjalną dokumentację języka, gdzie znajdziesz więcej szczegółów na temat działania funkcji readFile(). Jest także wiele przydatnych tutoriali dostępnych online, które mogą pomóc Ci w bardziej zaawansowanych zastosowaniach, takich jak przetwarzanie większych plików tekstowych czy obsługa wyjątków przy czytaniu pliku. Warto także zapoznać się z innymi narzędziami, takimi jak BufferedReader czy Scanner, które mogą ułatwić pracę z plikami tekstowymi w Kotlinie.

## Zobacz także

- Oficjalna dokumentacja języka Kotlin: https://kotlinlang.org/docs/reference/input-output.html#file-io
- Tutorial o czytaniu plików tekstowych w Kotlinie: https://www.javatpoint.com/kotlin-read-file
- Przykłady wykorzystania funkcji readFile(): https://thetechnocafe.com/how-to-read-a-text-file-in-kotlin/