---
title:                "Otrzymywanie aktualnej daty"
html_title:           "C#: Otrzymywanie aktualnej daty"
simple_title:         "Otrzymywanie aktualnej daty"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobranie bieżącej daty jest bardzo powszechnym zadaniem w programowaniu C#. Jest to przydatne w wielu przypadkach, na przykład w tworzeniu aplikacji z funkcją rejestrowania czasu, wyświetlania ostatnio modyfikowanych plików lub tworzenia raportów z datą wykonania. Poniżej zaprezentujemy kilka sposobów, jak można uzyskać bieżącą datę w C#.

## Jak?

```C#
DateTime currentDate = DateTime.Now; //Przypisanie bieżącej daty do zmiennej "currentDate"
Console.WriteLine(currentDate); //Wyświetlenie bieżącej daty w konsoli
```

W powyższym przykładzie użyliśmy obiektu `DateTime`, który jest wbudowany w język C# i pozwala nam na obsługę dat i godzin. Metoda `Now()` zwraca bieżącą datę i czas, a następnie możemy ją wyświetlić w dowolny sposób, np. za pomocą metody `WriteLine()` z klasy `Console`.

Możemy również sformatować wyświetloną datę za pomocą metody `ToString()` i podając odpowiednie parametry jako argumenty. Na przykład:

```C#
Console.WriteLine(currentDate.ToString("dddd, dd MMMM yyyy")); // Wyświetli "wtorek, 16 marca 2021"
```

Każdy z parametrów w nawiasach okrągłych określa kolejno: dzień tygodnia (po polsku), dzień miesiąca, nazwę miesiąca (po polsku) i rok. Pełną listę możliwych parametrów można znaleźć w dokumentacji języka C#.

## Deep Dive

Jeśli chcemy uzyskać bieżącą datę z większą precyzją, możemy użyć struktury `DateTimeOffset`. Jest to rozszerzenie klasy `DateTime` i pozwala na przechowywanie dodatkowych informacji o strefie czasowej. Przykład użycia:

```C#
DateTimeOffset currentDateTimeOffset = DateTimeOffset.Now;
Console.WriteLine(currentDateTimeOffset); //Wyświetli np. "17.03.2021 12:47:15 +01:00"
```

Pole `Offset` wyświetla przesunięcie czasowe między naszą strefą, a strefą czasową UTC (Coordinated Universal Time). W przykładzie, zakładając że mieszkamy w strefie czasowej GMT+1, wynik będzie 1-godzinny.

## Zobacz też:
- Dalsze możliwości manipulacji datami w C#: [https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-5.0)
- Dokumentacja klasy `DateTimeOffset`: [https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-5.0](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetimeoffset?view=net-5.0)
- Przewodnik po formatowaniu dat i godzin w C#: [https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings?view=net-5.0](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/custom-date-and-time-format-strings?view=net-5.0)