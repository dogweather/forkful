---
date: 2024-01-26 00:51:04.246132-07:00
description: "Jak to zrobi\u0107: Zacznijmy od bloku try-catch. To jak umieszczenie\
  \ siatki bezpiecze\u0144stwa pod linoskoczkiem. Je\u015Bli si\u0119 po\u015Blizgnie,\
  \ nie spada \u2014 zostaje\u2026"
lastmod: '2024-03-13T22:44:35.415750-06:00'
model: gpt-4-1106-preview
summary: Zacznijmy od bloku try-catch.
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Jak to zrobić:
Zacznijmy od bloku try-catch. To jak umieszczenie siatki bezpieczeństwa pod linoskoczkiem. Jeśli się poślizgnie, nie spada — zostaje złapany.

```C#
using System;

class PrzykladObslugiBledow {
    static void Main() {
        try {
            int[] liczby = {1, 2, 3};
            Console.WriteLine(liczby[5]);  // Ups, indeks poza zakresem!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Złapano błąd: " + e.Message);
        }
    }
}
```

Przykładowy wynik, gdy coś pójdzie nie tak:
```
Złapano błąd: Index był poza granicami tablicy.
```

Teraz dodajemy blok finally — dzieje się to niezależnie od wszystkiego, tak jak płacenie podatków.

```C#
try {
    // Potencjalnie problematyczny kod tutaj
} catch (SomeSpecificException e) {
    // Tutaj obsługujemy ten konkretny błąd
} finally {
    // Ten kod wykona się niezależnie od tego, co wydarzy się powyżej
    Console.WriteLine("To zawsze się wykona.");
}
```

## Głębsze zanurzenie
Obsługa błędów jest w C# od momentu jego powstania. Z biegiem czasu ewoluowała. Kiedyś programiści polegali na kodach powrotu czy globalnych flagach, żeby sygnalizować problemy — niewygodne i podatne na błędy.

C# używa wyjątków, bardziej nowoczesnego podejścia. Wyjątek jest wyrzucany, gdy zdarzy się coś niespodziewanego, zupełnie jak rzucenie flagi na boisku w futbolu amerykańskim. Strukturalna obsługa wyjątków za pomocą bloków try, catch i finally sprawia, że zarządzanie tymi momentami jest jaśniejsze i czystsze niż stare metody sprawdzania błędów.

Alternatywy? Pewnie. Jest `UnhandledExceptionEventHandler` dla wyjątków, które przeciekają. Albo w kodzie asynchronicznym, obsługa błędów staje na głowie z obiektami `Task`, które noszą własny bagaż wyjątków.

Detale implementacji — podobne do drobnego druku — mają znaczenie. Wyjątki mogą być kosztowne, obniżając wydajność, jeśli są rzucone bez opamiętania. Dlatego używamy ich w wyjątkowych przypadkach, a nie do codziennej kontroli logiki.

## Zobacz również
- [Oficjalna dokumentacja na temat Wyjątków w C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Najlepsze praktyki w obsłudze wyjątków C#](https://docs.microsoft.com/pl-pl/dotnet/standard/exceptions/best-practices-for-exceptions)
