---
title:                "Obliczanie daty w przyszłości lub w przeszłości"
html_title:           "C#: Obliczanie daty w przyszłości lub w przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub w przeszłości"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Planowanie jest ważną częścią naszego życia. Czasami musimy przewidzieć przyszłe daty, na przykład datę kolejnego spotkania lub ważne wydarzenie. W takich sytuacjach pomocne może być obliczanie daty w przyszłości lub przeszłości. Dzięki temu możemy lepiej zorganizować nasze obowiązki i uniknąć niepotrzebnego stresu.

## Jak to zrobić

Obliczanie daty w przyszłości lub przeszłości jest możliwe dzięki wykorzystaniu klasy ```DateTime``` w C#. Aby przewidzieć datę w przyszłości, należy dodać określoną ilość dni, tygodni, miesięcy lub lat do aktualnej daty. Natomiast, aby otrzymać datę w przeszłości, musimy odjąć odpowiednią ilość czasu. Przykładowy kod wyglądałby następująco:

```C#
// Przykład obliczenia daty za 7 dni
DateTime dzisiaj = DateTime.Today;
DateTime zaTydzien = dzisiaj.AddDays(7); 
Console.WriteLine("Data za 7 dni to: " + zaTydzien);
// Wynik: Data za 7 dni to: 10.06.2021 00:00:00
```

```C#
// Przykład obliczenia daty przed 1 miesiącem
DateTime dzisiaj = DateTime.Today;
DateTime przedMiesiacem = dzisiaj.AddMonths(-1); 
Console.WriteLine("Data przed 1 miesiącem to: " + przedMiesiacem);
// Wynik: Data przed 1 miesiącem to: 04.05.2021 00:00:00
```

## Deep Dive

Klasa ```DateTime``` oferuje również wiele innych metod, dzięki którym możemy precyzyjnie manipulować datami. Na przykład, możemy użyć metody ```AddHours()``` aby dodać lub ```SubtractMinutes()``` aby odjąć konkretne godziny lub minuty. W przypadku bardziej zaawansowanych obliczeń, możemy skorzystać z klasy ```TimeSpan```, która pozwala na określenie dokładnego interwału czasu, jaki chcemy dodać lub odjąć od daty.

## Zobacz też

- [Dokumentacja klasy DateTime w C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-5.0)
- [Artykuł: Podstawy programowania w C#](https://codecademy.com/articles/learn-c-sharp)