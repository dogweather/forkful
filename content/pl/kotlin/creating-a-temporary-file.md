---
title:                "Kotlin: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Tworzenie pliku tymczasowego jest przydatne w sytuacjach, gdy potrzebujemy tymczasowego przechowywania danych w naszym programie. Mogą one służyć jako miejsce do zapisywania tymczasowych danych lub jako kopia zapasowa dla ważnych plików.

## Jak to zrobić?

Tworzenie pliku tymczasowego w języku Kotlin jest bardzo proste. Wystarczy użyć klasy File z pakietu java.io i wykorzystać metodę createTempFile, podając nazwę pliku i rozszerzenie jako argumenty. Na przykład:

```Kotlin
val tempFile = File.createTempFile("example", ".txt")
println(tempFile.absolutePath)
```

Po uruchomieniu tego kodu, w konsoli zostanie wyświetlona ścieżka do utworzonego pliku tymczasowego. Domyślnie zostanie on zapisany w systemowym katalogu tymczasowym.

Możemy również określić własny katalog tymczasowy, podając ścieżkę do niego jako trzeci argument metody createTempFile. Na przykład:

```Kotlin
val tempFile = File.createTempFile("example", ".txt", "C:\\temp")
```

Jeśli potrzebujemy tylko stworzyć plik tymczasowy bez zapisywania go na dysku, możemy użyć metody File.createTempFile z jednym argumentem - prefiksem dla nazwy pliku. Na przykład:

```Kotlin
val tempFile = File.createTempFile("example", "")
println(tempFile.absolutePath)
```

W tym przypadku, plik tymczasowy zostanie utworzony w domyślnym katalogu tymczasowym, ale nie zostanie zapisany na dysku.

## Deep Dive

Tworząc plik tymczasowy, możemy użyć opcjonalnego argumentu prefix, który definiuje prefiks dla nazwy pliku tymczasowego. Jeśli nie zostanie on podany, domyślnym prefiksem jest "kotlin". Możemy również podać opcjonalny argument suffix, który definiuje rozszerzenie dla nazwy pliku. W przypadku gdy nie zostanie on podany, domyślne rozszerzenie to ".tmp".

Pliki tymczasowe są automatycznie usuwane, gdy nasz program zostanie zakończony, jednak możemy również usunąć je wcześniej przy użyciu metody delete na obiekcie klasy File.

## Zobacz również

- Interaktywny tutorial języka Kotlin: [link](https://kotlinlang.org/docs/tutorials/koans.html)
- Dokumentacja pakietu java.io: [link](https://docs.oracle.com/javase/7/docs/api/java/io/package-summary.html)
- Przewodnik po korzystaniu z Kotlin w Androidzie: [link](https://developer.android.com/kotlin)