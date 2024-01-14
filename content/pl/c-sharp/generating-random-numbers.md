---
title:    "C#: Generowanie losowych liczb"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach generowanie losowych liczb jest jednym z najważniejszych zadań programistycznych, wykorzystywanym w wielu aplikacjach. Bez tego procesu wiele funkcji i algorytmów nie byłoby w stanie działać. Dowiedz się, dlaczego warto poznać tajniki generowania losowych liczb w języku C#.

## Jak to zrobić

Generowanie losowych liczb jest jedną z podstawowych operacji w programowaniu. W języku C# możemy wykorzystać do tego celu kilka różnych metod. Oto jeden prosty przykład generowania losowej liczby całkowitej z zakresu od 0 do 100:

```C#
Random random = new Random();
int randomNumber = random.Next(0, 101);
Console.WriteLine(randomNumber);
```

W powyższym przykładzie wykorzystaliśmy klasę Random, która dostarcza nam funkcji do generowania liczb losowych. Metoda Next() przyjmuje dwa argumenty - początek i koniec zakresu, z którego ma zostać wygenerowana liczba. Dzięki temu możemy precyzyjnie kontrolować, jakie wartości będą generowane. Warto także pamiętać, że w celu uniknięcia powtarzających się wyników, należy utworzyć tylko jeden obiekt klasy Random i wywoływać na nim metodę Next() za każdym razem, gdy potrzebujemy nowej losowej liczby.

## Wnikliwe studium

Generowanie losowych liczb nie jest tak proste, jak mogłoby się wydawać. Klasy takie jak Random wykorzystują algorytmy, które opierają się na pierwotnym ziarnie, nazywanym także nasionem (seed). Warto wiedzieć, że to ziarno determinuje kolejność, w jakiej generowane są liczby. Dzięki temu możemy uzyskać tak zwane "pseudo-losowe" liczby, które w rzeczywistości są całkowicie deterministyczne. To ważne, ponieważ w niektórych przypadkach, na przykład w symulacjach komputerowych, nie chcemy uzyskać zupełnie losowych wartości, a jedynie kolejnych liczb, które wyglądają na losowe.

## Zobacz także

- [Dokumentacja klasy Random w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.random?view=netcore-3.1)
- [Artykuł o generowaniu liczb losowych na stronie GeeksforGeeks](https://www.geeksforgeeks.org/generating-random-numbers-in-c-sharp/)
- [Kurs online na platformie Codecademy dotyczący pracy z klasami i obiektami w C#](https://www.codecademy.com/learn/learn-c-sharp)