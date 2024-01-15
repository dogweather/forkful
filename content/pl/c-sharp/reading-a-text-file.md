---
title:                "Odczytywanie pliku tekstowego"
html_title:           "C#: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak programy czytają tekstowe pliki? Może chcesz napisać własny program, który będzie analizować dane zebranie w pliku tekstowym? W obu przypadkach konieczna jest umiejętność czytania plików tekstowych w języku C#. W tym artykule dowiesz się, dlaczego jest to ważna umiejętność i jak ją wykorzystać.

## Jak to zrobić

```c#
using System; // importujemy bibliotekę System, aby móc używać funkcji do czytania plików

string path = @"C:\Users\user\Desktop\dane.txt"; // ścieżka do pliku, którego chcemy odczytać
string[] lines = System.IO.File.ReadAllLines(path); // odczytujemy wszystkie linie z pliku i zapisujemy je jako tablicę linii

foreach (string line in lines) // iterujemy przez każdą linię w tablicy
{
    Console.WriteLine(line); // wyświetlamy każdą linię na ekranie
}

// Wyjściem programu będzie wyświetlenie wszystkich linii z pliku na ekranie.
```

## Głębszy zanurkuj

Oprócz funkcji `ReadAllLines()`, istnieją też inne metody do czytania plików tekstowych w języku C#. Na przykład, możemy użyć funkcji `ReadAllText()`, aby odczytać całą zawartość pliku jako pojedynczy ciąg znaków. Możemy także użyć funkcji `ReadLine()`, aby odczytać kolejną linię z pliku i przechodzić przez plik w ten sposób.

Jest również ważne, aby pamiętać o zamknięciu pliku po jego odczytaniu lub zapisaniu. Możemy to zrobić za pomocą funkcji `Close()` lub `Dispose()`. Można również użyć bloku `using`, który automatycznie zamknie plik po wykonaniu kodu wewnątrz bloku.

## Zobacz także

- [Dokumentacja C# - Przestrzeń nazw System.IO](https://docs.microsoft.com/pl-pl/dotnet/api/system.io?view=netcore-3.1)
- [How To Read/Write Text Files In C#](https://www.c-sharpcorner.com/article/reading-and-writing-text-file-in-C-Sharp/) (ang.)