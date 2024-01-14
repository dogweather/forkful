---
title:                "C#: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek musiałeś przetwarzać spory plik tekstowy w swoim programie? Czy zastanawiałeś się, jak usprawnić ten proces? W tym artykule dowiesz się, dlaczego warto poznać technikę czytania plików tekstowych oraz jak to zrobić w języku C#.

## Jak To Zrobić

Kodowanie w języku C# jest bardzo wygodne dzięki wbudowanym narzędziom do operacji na plikach. Przykładowo, aby odczytać zawartość pliku tekstowego, potrzebujemy tylko kilku linijek kodu:

```C#
using System.IO; // importujemy bibliotekę do operacji na plikach

string path = @"C:\nazwa_folderu\plik.txt"; // określamy ścieżkę pliku
string content = File.ReadAllText(path); // odczytujemy jego zawartość

Console.WriteLine(content); // wyświetlamy zawartość na ekranie
```

W powyższym przykładzie korzystamy z metody `ReadAllText` z klasy `File`, która przypisuje całą zawartość pliku do zmiennej `content`. Następnie wyświetlamy ją na ekranie dzięki funkcji `WriteLine` z klasy `Console`.

## Głębsze Zagadnienia

Warto zauważyć, że w powyższym przykładzie nie musimy pamiętać o zamykaniu pliku - metoda `ReadAllText` robi to za nas. Możemy także sprecyzować kodowanie czy znaki nowej linii przy pomocy innych metod, takich jak `ReadAllLines` czy `ReadAllBytes`.

Kolejną przydatną funkcją jest operowanie na poszczególnych linijkach w pliku. Możemy to zrobić przy pomocy metody `File.ReadAllLines(path)`, która zwraca tablicę stringów zawierającą wszystkie linie pliku. Możemy także użyć pętli `foreach` do przejścia przez te linie lub wybierać konkretne linie według indeksów.

## Zobacz Również

Jeśli chcesz dowiedzieć się więcej o operacjach na plikach w języku C#, możesz zapoznać się z poniższymi artykułami:

- [Dokumentacja Microsoft o klasie `File`](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.file?view=netcore-3.1)
- [Poradnik Stack Overflow dotyczący czytania pliku w C#](https://stackoverflow.com/questions/1393708/how-to-read-a-text-file/1393734#1393734)

Teraz już wiesz, jak odczytać zawartość pliku tekstowego w języku C#. Mamy nadzieję, że ten artykuł był dla Ciebie pomocny. Powodzenia!