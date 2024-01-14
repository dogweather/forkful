---
title:                "C#: Znajdywanie długości ciągu znaków"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego:

Często przy pisaniu kodu w języku C# zdarza się nam potrzeba znalezienia długości ciągu znaków. Może to być konieczne, aby upewnić się, że wprowadzane dane są odpowiedniej długości lub aby przetwarzać je w dalszej części programu. W tym artykule dowiesz się, jak znaleźć długość ciągu znaków w C#.

## Jak to zrobić:

W języku C# istnieje kilka sposobów na znalezienie długości ciągu znaków. Jednym z nich jest użycie metody Length, która jest dostępna dla typu danych string. Przykładowy kod wyglądałby następująco:

```C#
string imie = "Kasia";
int dlugosc = imie.Length;
Console.WriteLine("Długość imienia to " + dlugosc + " znaków.");
```

Output: *Długość imienia to 5 znaków.*

Innym sposobem jest użycie metody ToCharArray, która zamienia ciąg znaków na tablicę typu char. Następnie można wykorzystać właściwość Length dla tej tablicy, aby uzyskać długość ciągu. Przykładowy kod wyglądałby następująco:

```C#
string nazwisko = "Nowak";
char[] tablica = nazwisko.ToCharArray();
int dlugosc = tablica.Length;
Console.WriteLine("Długość nazwiska to " + dlugosc);
```

Output: *Długość nazwiska to 5.*

## Deep Dive:

W tle obu metod znajduje się właściwie taka sama logika. W przypadku użycia metody Length dla typu string, najpierw musimy przekonwertować ciąg znaków na tablicę char, a następnie liczyć elementy w tej tablicy. Metoda ToCharArray wykonuje te same kroki, ale dodatkowo tworzy nową tablicę, co może spowodować pewne narzuty wydajnościowe w przypadku dużych ciągów.

Inną ciekawą rzeczą jest to, że właściwość Length w języku C# nie jest metodą, ale polem, co oznacza, że nie wymaga ona nawiasów po jej nazwie. Jest to tzw. pole zdefiniowane i jest to właśnie ono, które służy do przechowywania informacje o długości ciągu znaków.

## Zobacz też:

- [Dokumentacja języka C#](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [Tutorial: String Length and Substrings in C#](https://www.educative.io/edpresso/string-length-and-substrings-in-csharp)