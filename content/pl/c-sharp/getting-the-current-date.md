---
title:                "C#: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Obecna data i godzina są niezwykle ważne w programowaniu. Nie tylko pozwala to na śledzenie postępu działania programu, ale również na wyświetlanie aktualnych informacji dla użytkownika. Dlatego warto poznać sposoby pobierania bieżącej daty za pomocą języka C#.

## Jak to zrobić

```C#
// Pobranie bieżącej daty i godziny
DateTime now = DateTime.Now;
Console.WriteLine(now);

// Pobranie tylko daty
DateTime currentDate = DateTime.Now.Date;
Console.WriteLine(currentDate);

// Pobranie tylko godziny
DateTime currentTime = DateTime.Now.TimeOfDay;
Console.WriteLine(currentTime);
```

W powyższych przykładach wykorzystaliśmy klasę DateTime, która jest wbudowana w język C#. Aby uzyskać bieżącą datę i godzinę, wystarczy wywołać metodę `Now()` na tej klasie. Możemy również wykorzystać metody `Date` i `TimeOfDay`, aby uzyskać tylko datę lub godzinę.

## Dogłębna analiza

Klasa DateTime oferuje wiele innych metod, które pomogą w manipulacji danymi dotyczącymi daty i godziny. Na przykład, możemy użyć metod `Add()` lub `Substract()` do dodawania lub odejmowania określonej liczby dni, godzin lub minut od bieżącej daty. Możemy też porównywać daty za pomocą metod `Equals()` lub `CompareTo()`. W przypadku pracy z międzynarodowymi danymi, warto wykorzystać metody `ToUniversalTime()` lub `ToLocalTime()` do przekształcania stref czasowych.

## Zobacz też

- [Dokumentacja Microsoft dla klasy DateTime](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=netcore-3.1)
- [Poradnik dla początkujących w programowaniu w języku C#](https://sjp.pl/C%23)
- [Kurs programowania w języku C# na platformie Codecademy](https://www.codecademy.com/learn/learn-c-sharp)