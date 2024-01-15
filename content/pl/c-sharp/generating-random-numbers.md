---
title:                "Generowanie losowych liczb"
html_title:           "C#: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Losowość jest nieodłączną częścią wielu aplikacji i systemów informatycznych. W przypadku tworzenia gier komputerowych lub symulacji, generowanie liczb losowych jest niezbędne do tworzenia różnych scenariuszy i zachowań. W programowaniu, losowe liczby mogą być również wykorzystywane do testowania funkcjonalności i wydajności programów.

## Jak

Generowanie liczb losowych w języku C# jest bardzo proste. Wystarczy skorzystać z klasy `Random`, która jest dostępna w przestrzeni nazw System. Najpierw należy utworzyć nowy obiekt tej klasy, a następnie użyć jednej z dostępnych metod do pobrania losowej liczby. Przykładowy kod wyglądałby następująco:

```C#
Random rnd = new Random(); //utworzenie obiektu Random
int randomNumber = rnd.Next(1, 10); //pobranie liczby losowej z zakresu od 1 do 10
Console.WriteLine("Wylosowana liczba to: " + randomNumber); //wyświetlenie wylosowanej liczby
```

Powyższy kod wygeneruje losową liczbę całkowitą z zakresu od 1 do 10 i wyświetli ją na ekranie.

Jeśli chcemy wygenerować liczbę zmiennoprzecinkową, możemy użyć metody `NextDouble()`:

```C#
Random rnd = new Random();
double randomDouble = rnd.NextDouble(); //pobranie losowej liczby zmiennoprzecinkowej
Console.WriteLine("Wylosowana liczba zmiennoprzecinkowa to: " + randomDouble);
```

Warto również wspomnieć, że klasa `Random` posiada wiele innych przydatnych metod, takich jak `NextBytes()` do generowania losowych ciągów bajtów czy `NextBytes(min, max)`, dzięki której możemy wygenerować losową liczbę z określonego zakresu.

## Deep Dive

Podczas tworzenia aplikacji, gdzie losowość jest kluczowa, warto zwrócić uwagę na kilka ważnych aspektów dotyczących generowania liczb losowych w C#.

Po pierwsze, ważne jest, aby utworzyć tylko jeden obiekt klasy `Random` i korzystać z niego do generowania wszystkich liczb w naszym programie. W przeciwnym razie, jeśli będziemy tworzyć nowe obiekty przy każdym wywołaniu, możemy otrzymać nieoczekiwane wyniki lub nawet pętlę nieskończoną.

Kolejną ważną kwestią jest ustawienie ziarna (seed) dla generatora liczb losowych. Domyślnie, ziarno jest ustawione na podstawie bieżącego czasu, co oznacza, że jeśli uruchomimy nasz program w tym samym momencie, otrzymamy te same wyniki. Aby temu zapobiec, możemy ustawić własne ziarno, na przykład używając wartości zegara systemowego lub losowej liczby.

Ponadto, klasa `Random` nie jest w pełni losowa, a jedynie wykorzystuje funkcję matematyczną do generowania ciągu liczb pseudolosowych. Dlatego, jeśli wymagamy wysokiej jakości losowości, warto rozważyć użycie innych bibliotek lub algorytmów do generowania liczb losowych.

## Zobacz także

- Dokumentacja klasy `Random` w języku C# (https://docs.microsoft.com/en-us/dotnet/api/system.random)
- Porównanie wydajności różnych metod generowania liczb losowych w C# (https://timetler.com/en/which-c-number-random-generation-is-the-most-efficient/)
- Przykładowe zastosowanie generowania liczb losowych w grach wideo (https://www.gamasutra.com/blogs/SethStahl/20150429/241849/