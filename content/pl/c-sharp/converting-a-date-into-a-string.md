---
title:                "C#: Konwertowanie daty na ciąg znaków"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest częstym zadaniem w programowaniu, szczególnie w języku C#. Pozwala ona na wyświetlenie daty w czytelnej formie dla użytkownika, na przykład w formie tekstowej w interfejsie użytkownika programu. W tym artykule dowiesz się jak dokonać tej konwersji w C#, oraz przeczytasz o kilku ciekawych szczegółach z nią związanych.

## Jak to zrobić

Aby konwertować datę na ciąg znaków, możemy skorzystać z poniższego kodu:

```c#
DateTime date = DateTime.Now; //Tworzymy zmienną zawierającą aktualną datę
string stringDate = date.ToString("dd-MM-yyyy"); //Wywołujemy metodę ToString(), podając jako argument format daty, w którym chcemy ją wyświetlić
Console.WriteLine(stringDate); //Wypisujemy wynik w konsoli
```

Powyższy kod utworzy zmienną `stringDate`, która będzie zawierać bieżącą datę w formacie *DD-MM-RRRR*, na przykład *12-08-2021*. Możemy dowolnie zmieniać format daty, używając różnych kombinacji liter, na przykład *MM/dd/RRRR* (08/12/2021) lub *RRRR/MM/dd* (2021/08/12).

## Mocniejsze zagłębienie

Podczas konwertowania daty na ciąg znaków, warto wiedzieć o kilku ciekawych szczegółach. Po pierwsze, możemy użyć metody `.ToString()` nie tylko na zmiennych typu `DateTime`, ale również na innych typach, takich jak `int` czy `double`. Po drugie, możemy użyć specjalnych znaków, takich jak `"/"` czy `"-"`, aby podzielić datę na poszczególne części, takie jak dzień, miesiąc, rok itp.

Jednym z bardziej zaawansowanych zastosowań konwersji daty na ciąg znaków jest porównywanie dat. Dzięki konwersji na taki sam format (na przykład *DD-MM-RRRR*), możemy łatwo porównywać daty jako ciągi znaków, używając standardowych operatorów porównania (`<`, `>`, `==` itp.).

## Zobacz również

- [Dokumentacja Microsoft na temat konwersji daty na ciąg znaków](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Przewodnik po formatach dat w C#](https://csharp.today/przewodnik-po-formatach-dat-w-c/)
- [Przydatne triki i wskazówki programistyczne w C#](https://jaksiemasz.com/6-przydatnych-trikow-w-c-sharp/)