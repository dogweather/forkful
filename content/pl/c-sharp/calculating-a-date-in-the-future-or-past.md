---
title:                "C#: Obliczanie daty w przeszłości lub przyszłości"
simple_title:         "Obliczanie daty w przeszłości lub przyszłości"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach niezliczone ilości danych są przechowywane i przetwarzane w formie cyfrowej. Jedną z często spotykanych potrzeb jest obliczanie daty w przyszłości lub w przeszłości. Może to wynikać z różnych powodów, takich jak przewidywanie wyników biznesowych lub planowanie wydarzeń osobistych. W tym artykule dowiecie się, jak w prosty sposób wykonać te obliczenia za pomocą języka C#.

## Jak to zrobić

Pierwszym krokiem jest zrozumienie formatu daty w języku C#. Wartość daty jest przechowywana w formacie DateTime, który zawiera informacje o dniu, miesiącu, roku, godzinie, minucie i sekundzie. Można go zainicjować za pomocą następującego kodu:

```C#
DateTime data = new DateTime(rok, miesiąc, dzień, godzina, minuta, sekunda);
```

Warto zauważyć, że miesiące są liczone od 1 do 12, a dni od 1 do 31, z wyjątkiem lutego, który może mieć 28 lub 29 dni w przypadku roku przestępnego.

Teraz, gdy mamy zainicjalizowany obiekt daty, możemy wykonać obliczenia w przyszłości lub w przeszłości. Do tego celu użyjemy metody Add, która przyjmuje jako argument TimeSpan, czyli przedział czasu. Przykładowo, aby dodać miesiąc do daty, możemy użyć kodu:

```C#
DateTime przyszlaData = data.AddMonths(1);
```

Możliwe jest również odejmowanie czasu z daty, wystarczy dodać przedrostek minus. Przykładowo, aby odjąć 2 lata, możemy użyć kodu:

```C#
DateTime przeszlaData = data.AddYears(-2);
```

Możemy również dokonać obliczeń bardziej precyzyjnych, na przykład dodając tylko dni lub godziny. Warto zapoznać się ze wszystkimi dostępnymi metodami w dokumentacji języka C#.

## Deep Dive

Podczas wykonywania obliczeń z datami warto zwrócić uwagę na fakt, że niektóre operacje mogą prowadzić do błędów. Na przykład dodawanie miesięcy lub lat może spowodować zmianę miesiąca lub roku, co może nie być oczekiwane. Dlatego warto pamiętać, że niektóre obliczenia mogą być bardziej skomplikowane niż się na początku wydaje.

Jednym z przydatnych narzędzi jest również klasa TimeSpan, która pozwala na bardziej precyzyjne operacje na przedziałach czasu. Można ją wykorzystać do obliczenia różnicy między dwiema datami lub utworzenia własnego przedziału czasu.

## Zobacz również

- [Dokumentacja języka C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/)
- [Poradnik dla początkujących w języku C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/tutorials/)
- [Przykłady wykorzystania dat w języku C#](https://www.c-sharpcorner.com/uploadfile/991e9f/manipulating-dates-and-times-using-C-Sharp/)
- [Kolekcja przydatnych narzędzi w języku C#](https://www.c-sharpcorner.com/article/C-Sharp-toolkit/)