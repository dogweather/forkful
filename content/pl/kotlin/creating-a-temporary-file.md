---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Kotlin: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Tworzenie tymczasowych plików jest bardzo przydatne, gdy potrzebujemy przechowywać dane tymczasowo, na przykład w przypadku przetwarzania large danych lub gdy korzystamy z zewnętrznych bibliotek, które wymagają dostępu do plików.

## Jak to zrobić?

```Kotlin
val tempFile = File.createTempFile("temporary", ".txt")
tempFile.writeText("To jest przykładowy tekst.")
println(tempFile.name) // zwraca nazwę tymczasowego pliku, na przykład: temporary4502620670960386395.txt
tempFile.delete()
```

Tak prosto możemy utworzyć tymczasowy plik w języku Kotlin. Początkowo określamy prefiks i sufiks nazwy pliku, a następnie możemy wykonać operacje na tym pliku, takie jak zapisywanie lub odczytywanie danych. Pamiętajmy jednak o usunięciu pliku po jego użyciu, aby nie pozostawiać niepotrzebnych śmieci na naszym urządzeniu.

## Zanurzenie się w temat

Tymczasowe pliki są tworzone w systemowym folderze tymczasowym, więc nie musimy sami określać ścieżki do pliku. Możemy również określić wybrany folder jako miejsce przechowywania tymczasowych plików, używając metody `createTempFile()` z argumentami `prefix`, `suffix` oraz `directory`.

Dodatkowo, możemy określić, czy plik ma być tylko do odczytu lub weryfikować jego istnienie za pomocą metod `isReadOnly()` i `exists()`.

## Zobacz także

- Dokumentacja Javy dla klasy [File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- Instrukcja języka Kotlin dla funkcji [createTempFile()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)