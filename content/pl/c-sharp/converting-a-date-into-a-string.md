---
title:    "C#: Konwertowanie daty na ciąg znakowy"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego?

Konwersja daty na ciąg znaków jest niezbędną czynnością w wielu programach. Jest to często wykorzystywana operacja przy tworzeniu aplikacji mobilnych, stron internetowych, a także w wielu innych dziedzinach programowania. W tym artykule omówimy, jak w łatwy sposób dokonać konwersji daty na string w języku C#.

## Jak to zrobić?

```C#
// Przykładowa data
DateTime date = new DateTime(2020, 10, 24);

// Konwersja daty na string w formacie dd/MM/yyyy
string dateString = date.ToString("dd/MM/yyyy");

// Przykładowy output: "24/10/2020"
Console.WriteLine(dateString);
```

Kod powyżej pokazuje, jak możliwe jest skonwertowanie daty na ciąg znaków w ustalonym formacie. Istnieje wiele różnych formatów, w których można wyświetlić datę. Kilka innych przykładów wykorzystania metody `ToString()`:

- `date.ToString("dd-MM-yyyy")`: "24-10-2020"
- `date.ToString("M/d/yyyy")`: "10/24/2020"
- `date.ToString("MMMM dd, yyyy")`: "October 24, 2020"

Można także połączyć formatowanie z czasem, na przykład: `date.ToString("dd/MM/yyyy HH:mm")` da output "24/10/2020 00:00".

## Głębsza analiza

Metoda `ToString()` jest wywoływana na zmiennej typu `DateTime` i przyjmuje jako argument formatowanie daty w postaci stringa. Istnieją jednak inne sposoby wykonania konwersji daty na string w języku C#, np. wykorzystując klasę `Convert` lub metodę `ToString()` z wykorzystaniem formatu standardowego, np. `date.ToString("u")` dla formatu unikodowego.

Ważne jest również zwrócenie uwagi na lokalizację i ustawienia kulturowe, gdyż może to mieć wpływ na wyświetlany format daty. W przypadku potrzeby bardziej precyzyjnej kontroli nad wyświetlanym formatem daty, warto zapoznać się z dokumentacją języka C#.

## Zobacz także

- [Dokumentacja String.Format() w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.format?view=netcore-3.1)
- [Konwersja daty na string z wykorzystaniem metody ToString()](https://www.c-sharpcorner.com/blogs/convert-date-to-string-in-c-sharp-programming)