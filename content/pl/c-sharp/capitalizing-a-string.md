---
title:    "C#: Zmiana wielkości litery w ciągu znaków."
keywords: ["C#"]
---

{{< edit_this_page >}}

# Dlaczego powinieneś zainteresować się formatowaniem tekstu w języku C#?

Jeśli jesteś początkującym programistą, może wydawać ci się, że formatowanie tekstu to drobny i nieistotny szczegół. Jednak w rzeczywistości jest to bardzo użyteczna umiejętność, która może ułatwić wiele codziennych zadań programistycznych. Dlatego właśnie warto poznać możliwości formatowania tekstu w języku C#.

## Jak to zrobić?

Sposób formatowania tekstu w języku C# jest bardzo prosty i wykorzystuje metodę `ToUpper()` do konwersji czcionki na wielkie litery. Poniżej przedstawiam przykładowy kod w języku C#:

```C#
string imie = "jan";
string imieWielkimi = imie.ToUpper();

Console.WriteLine(imieWielkimi);
```

Output: JAN

Jak widać, wykorzystanie metody `ToUpper()` pozwala na szybką i łatwą zmianę formatowania tekstu. Możliwe jest również wykorzystanie innych metod, takich jak `ToLower()` czy `Replace()` do dalszej edycji tekstu.

## Głębszy zanurzenie

Istnieje wiele różnych sposobów na formatowanie tekstu w języku C#, w zależności od potrzeb i wymagań. Dzięki temu możemy dostosować wygląd tekstu do naszych potrzeb, co może być bardzo przydatne w wielu sytuacjach. Przykładowe zastosowania formatowania tekstu w języku C# to generowanie raportów, poprawa wyglądu interfejsu użytkownika czy wyświetlanie danych w czytelniejszy sposób.

## Zobacz także

- [Tutorial: Formatowanie tekstu w języku C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/formatting-types)
- [Przykłady wykorzystania formatowania tekstu w języku C#](https://www.c-sharpcorner.com/UploadFile/20abe7/coded-examples-for-string-formatting-in-Cs/)
- [Dokumentacja metody ToUpper() w języku C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)