---
title:                "Kotlin: Tworzenie pliku tekstowego"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## DlaczegoKotlin jest popularnym językiem programowania, ponieważ jest prosty w użyciu, bezpieczny i wydajny. Pisanie plików tekstowych jest zwykle potrzebnym elementem w wielu projektach programistycznych, zwłaszcza w celu przechowywania danych lub konfiguracji. W tym blogu dowiesz się, jak pisać pliki tekstowe w Kotlinie i jak może to ułatwić Twoją pracę.

## Jak to zrobić

Krok 1: Zaimportuj klasy potrzebne do pracy z plikami

```
import java.io.File
import java.io.FileOutputStream
```

Krok 2: Utwórz obiekt File, który będzie reprezentować Twoją nazwę pliku, określając ścieżkę i nazwę pliku

```
val fileName = File("ścieżka/plik.txt")
```

Krok 3: Utwórz nowy obiekt FileOutputStream, który będzie używany do zapisywania danych do pliku

```
val fileOutputStream = FileOutputStream(fileName)
```

Krok 4: Stwórz tekst, który chcesz zapisać do pliku

```
val text = "To jest przykładowy tekst, który zostanie zapisany do pliku."
```

Krok 5: Użyj metody write na obiekcie FileOutputStream, aby zapisać tekst jako tablicę bajtów

```
fileOutputStream.write(text.toByteArray())
```

Krok 6: Zamknij obiekt FileOutputStream, aby zakończyć zapisywanie

```
fileOutputStream.close()
```

Krok 7: Sprawdź, czy plik został utworzony, wywołując metodę exists na obiekcie File

```
if (fileName.exists()) {
    println("Plik został utworzony.")
} else {
    println("Nie udało się utworzyć pliku.")
}
```

## Bardziej szczegółowo

W Kotlinie istnieją inne metody, które mogą ułatwić pisanie plików tekstowych. Możesz na przykład użyć funkcji writeText, która automatycznie tworzy obiekt File i obiekt FileOutputStream, a następnie zapisuje tekst w wyznaczonej ścieżce. Możesz również użyć funkcji appendText, aby dopisywać tekst do istniejącego pliku, zamiast tworzyć nowy.

Inną przydatną funkcją jest readText, która pozwala na wygodne odczytywanie danych z pliku tekstowego i zapisanie ich w zmiennej tekstowej. Możesz także użyć funkcji readLines, aby odczytać plik jako listę linii.

## Zobacz również

- Dokumentacja Kotlin na temat pisanie plików: https://kotlinlang.org/docs/tutorials/kotlin-for-py/writing-files.html
- Wprowadzenie do języka Kotlin: https://kotlinlang.org/docs/reference/basic-syntax.html