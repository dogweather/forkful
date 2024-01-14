---
title:    "C#: Generowanie losowych liczb"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłącznym elementem programowania. Dzięki temu procesowi możemy symulować losowe zdarzenia lub tworzyć unikalne klucze, co jest niezbędne w wielu zastosowaniach. W tym artykule dowiesz się, jak w prosty sposób generować losowe liczby w języku C#.

## Jak To Zrobić

W języku C# istnieje kilka sposobów na generowanie losowych liczb, w zależności od naszych potrzeb.

### Metoda `Random`

Najprostszym sposobem jest użycie klasy `Random`, która jest już dostępna w standardowej bibliotece języka C#. Wystarczy utworzyć nowy obiekt tej klasy i wywołać metodę `Next()`, aby otrzymać losową liczbę całkowitą. Przykład:

```C#
var rnd = new Random();
var randomInt = rnd.Next(); // Wygeneruje losową liczbę całkowitą
```

Jeśli chcemy ograniczyć zakres generowanych liczb, możemy przekazać jako parametry metody `Next()` wartości `min` i `max`:

```C#
var rnd = new Random();
var randomInt = rnd.Next(1, 10); // Wygeneruje losową liczbę całkowitą z przedziału [1, 10)
```

### Metoda `Random.NextDouble()`

Jeśli potrzebujemy liczby zmiennoprzecinkowej, możemy skorzystać z metody `NextDouble()` klasy `Random`. Zwrócona wartość będzie znajdować się w przedziale [0, 1).

```C#
var rnd = new Random();
var randomDouble = rnd.NextDouble(); // Wygeneruje losową liczbę zmiennoprzecinkową z przedziału [0, 1)
```

### Metoda `Guid.NewGuid()`

Klasa `Guid` w języku C# służy do generowania unikalnych identyfikatorów, które są bardzo przydatne w niektórych aplikacjach. Jeśli potrzebujemy unikalnego klucza, możemy skorzystać z metody `NewGuid()`, która zwraca obiekt typu `Guid`. Przykład:

```C#
var uniqueKey = Guid.NewGuid(); // Wygeneruje unikalny identyfikator
```

## Wnikliwsze Spostrzeżenia

Dużo zależy od sposobu generowania liczb losowych. W języku C# można skorzystać z różnych algorytmów, z których każdy ma swoje wady i zalety. Dlatego, jeśli wykorzystujemy losowanie w aplikacji krytycznej, warto się dokładniej przyjrzeć dostępnym rozwiązaniom i wybrać to, które najlepiej spełni nasze wymagania.

## Zobacz Również

- [Dokumentacja klasy Random w języku C#](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [Porównanie różnych algorytmów generujących losowe liczby](https://medium.com/developers-writing/choosing-a-random-number-algorithm-in-c-2fe3e6df90d8)
- [Wykorzystanie generatorów liczb losowych w grach](https://gamedevelopment.tutsplus.com/tutorials/quick-tip-choosing-random-numbers-within-a-range--gamedev-12480)