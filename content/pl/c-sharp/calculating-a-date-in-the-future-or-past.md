---
title:    "C#: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Kalkulowanie dat przyszłych lub przeszłych jest częstym zadaniem w programowaniu i może być przydatne w wielu różnych sytuacjach. Może to być potrzebne do określenia, w jaki dzień tygodnia przypada dana data, lub do tworzenia przypomnień o ważnych wydarzeniach.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub w przeszłości w języku C#, możemy skorzystać z klasy DateTime. Przykład kodu wygląda następująco:

```C#
// Obliczenie daty w przyszłości
DateTime dzisiaj = DateTime.Today;
DateTime przyszlaData = dzisiaj.AddDays(7); //doda 7 dni do dzisiaj

// Obliczenie daty w przeszłości
DateTime pierwotnaData = new DateTime (2021, 7, 1);
DateTime przeszlaData = pierwotnaData.AddDays(-14); //odjęcie 14 dni od daty

// Wypisanie wyników
Console.WriteLine(przyszlaData); //wypisze np. "2021-07-08 00:00:00"
Console.WriteLine(przeszlaData); //wypisze np. "2021-06-17 00:00:00"
```

W powyższym przykładzie najpierw tworzymy obiekt klasy DateTime, a następnie używamy metody `AddDays` lub `SubtractDays` do dodawania lub odejmowania określonej liczby dni do lub od danej daty.

## Dogłębna analiza

Obliczanie daty w przyszłości lub przeszłości może wymagać uwzględnienia wielu różnych czynników, takich jak różnice w kalendarzach, różne sposoby formatowania dat w różnych krajach, czy obsługa przestępnych lat. Dlatego ważne jest, aby dobrze poznać i zrozumieć działanie klasy DateTime oraz innych kluczowych klas i metod związanych z obsługą dat w języku C#.

## Zobacz również

- Dokumentacja klasy DateTime w języku C# (https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime)
- Przykłady wykorzystania klasy DateTime (https://www.c-sharpcorner.com/UploadFile/mahesh/working-with-datetime-class-in-C-Sharp/)
- Poradnik dotyczący wykorzystania klasy DateTime (https://www.tutorialsteacher.com/csharp/csharp-datetime)