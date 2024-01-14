---
title:    "C#: Porównywanie dwóch dat"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest nieodłączną częścią programowania w języku C#, ponieważ często musimy porównywać różne daty w celu wykonania odpowiednich operacji. Jest to przydatna umiejętność, którą warto opanować, aby móc tworzyć bardziej kompleksowe i efektywne programy.

## Jak to zrobić

Aby porównać dwie daty w języku C#, musimy najpierw utworzyć dwie zmienne typu `DateTime`, które będą przechowywać nasze daty. Następnie używając operatorów porównania `>` (większe niż), `<` (mniejsze niż) lub `==` (równe), możemy porównać te daty i wykonać odpowiednie akcje w zależności od wyniku porównania.

```C#
DateTime data1 = new DateTime(2020, 10, 1);
DateTime data2 = new DateTime(2020, 11, 1);

if(data1 > data2)
{
    Console.WriteLine("Data 1 jest późniejsza niż data 2.");
}
else if(data1 < data2)
{
    Console.WriteLine("Data 1 jest wcześniejsza niż data 2.");
}
else
{
    Console.WriteLine("Obie daty są równe.");
}
```

Output:
```
Data 1 jest wcześniejsza niż data 2.
```

Możemy również używać metod `Compare()` oraz `Equals()` z klasy `DateTime` do porównywania dat. Metoda `Compare()` zwraca wartość większą niż 0, jeśli pierwsza data jest późniejsza, wartość mniejszą niż 0, jeśli jest wcześniejsza, oraz 0 w przypadku równości. Natomiast metoda `Equals()` zwraca wartość `true` lub `false` w zależności od wyniku porównania.

```C#
DateTime data1 = new DateTime(2020, 10, 1);
DateTime data2 = new DateTime(2020, 11, 1);
int wynikPorownania = DateTime.Compare(data1, data2);

Console.WriteLine("Wynik porównania: " + wynikPorownania);
Console.WriteLine("Czy daty są równe? " + data1.Equals(data2));
```

Output:
```
Wynik porównania: -1
Czy daty są równe? False
```

## Głębszy wgląd

W języku C# istnieje również wiele innych narzędzi i metod, które mogą pomóc nam w porównywaniu dat. Na przykład możemy porównywać tylko konkretny składnik daty, takie jak rok, miesiąc lub dzień, używając odpowiednich własności z klasy `DateTime`. Możemy także wykorzystać klasy `TimeSpan` i `DateTimeOffset` do porównywania różnic czasowych między dwoma datami. Istnieją również biblioteki zewnętrzne, takie jak Noda Time, które oferują bardziej zaawansowane i precyzyjne operacje na datach.

Znajomość różnych metod porównywania dat w języku C# jest ważna, ponieważ pozwala nam na dokładniejsze i bardziej elastyczne operacje na czasie w naszych programach.

## Zobacz też

- Dokumentacja C# dla klasy DateTime: https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=netframework-4.8
- Tutorial "Jak porównywać daty w C#": https://www.c-sharpcorner.com/article/how-to-compare-dates-in-c-sharp/
- Noda Time: https://nodatime.org/