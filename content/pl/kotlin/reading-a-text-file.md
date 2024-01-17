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

## Co to jest i dlaczego?
Czytasz wiele kodów i widzisz funkcję `readTextFile()`, ale nie wiesz, co to jest? Czy ktokolwiek tak naprawdę potrzebuje czytać plik tekstowy? Odpowiedź brzmi - tak! Programiści często muszą przetwarzać dane z plików tekstowych, ponieważ jest to popularny format plików do przechowywania i przesyłania informacji.

## Jak to zrobić:
Zobacz przykładowy kod w języku Kotlin poniżej, aby dowiedzieć się, jak odczytać plik tekstowy i wyświetlić jego zawartość:

```Kotlin
val plik = File("ścieżka/do/pliku.txt")
val zawartość = plik.readText()
println(zawartość)
```

**Wynik:** Zostanie wyświetlona zawartość pliku tekstowego.

## Głęboki zanurzyć:
Pierwsze biblioteki do manipulacji plikami tekstowymi pojawiły się już w latach 60-tych. Jednym z najpopularniejszych sposobów czytania plików tekstowych jest wykorzystanie funkcji `readText()`, która jest dostępna w większości języków programowania. Alternatywnym rozwiązaniem może być czytanie pliku wiersz po wierszu, co jest przydatne w przypadku dużych plików. Implementacja funkcji `readText()` jest oparta na buforowaniu, dzięki czemu odczyt wielu znaków na raz jest szybszy.

## Zobacz również:
- Dokumentacja języka Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html
- Poradnik dla początkujących: https://kotlinlang.org/docs/tutorials/kotlin-for-py/reading-files.html