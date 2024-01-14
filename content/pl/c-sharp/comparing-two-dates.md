---
title:                "C#: Porównywanie dwóch dat"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest bardzo ważną umiejętnością w programowaniu. Wiele aplikacji wymaga porównania dwóch dat, na przykład w celu wyświetlenia poinformowania użytkownika o ważnych wydarzeniach lub wyświetlenia danych w określonej kolejności.

## Jak to zrobić

Aby porównać dwie daty w języku C#, najpierw musimy utworzyć dwa obiekty typu DateTime. Następnie możemy wykorzystać różne metody, takie jak `Compare()` lub `Equals()`, aby porównać te obiekty i uzyskać wynik w postaci liczby całkowitej -1, 0 lub 1.

Przykładowy kod:

```C#
// Tworzenie dwóch obiektów typu DateTime
DateTime data1 = new DateTime(2021, 10, 1);
DateTime data2 = new DateTime(2021, 11, 1);

// Porównywanie dat
int wynik = data1.Compare(data2);

// Wyświetlanie wyniku
Console.WriteLine(wynik); // Output: -1
```

W powyższym przykładzie, wynik jest równy -1, ponieważ data1 jest wcześniejsza od data2. Możemy również użyć innych metod, takich jak `Equals()`, aby porównać daty na podstawie konkretnych wartości, na przykład dni tygodnia lub rok.

## Głębsze spojrzenie

Podczas porównywania dat ważne jest, aby zwrócić uwagę na czas i strefy czasowe. W przypadku porównywania dwóch obiektów typu DateTime, które mają różne strefy czasowe, wynik może być nieprzewidywalny. Aby uniknąć tego problemu, należy użyć metody `ToUniversalTime()`, która konwertuje czas lokalny na czas uniwersalny (UTC).

## Zobacz również

- Dokumentacja Microsoft na temat porównywania dat w języku C#: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-5.0
- Przydatny poradnik na temat manipulacji datami w C#: https://www.c-sharpcorner.com/article/date-time-manipulation-in-C-Sharp/