---
title:    "Swift: Generowanie losowych liczb"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią wielu aplikacji programistycznych. Może być wykorzystane m.in. w grach, symulacjach czy testach jednostkowych. W artykule dowiesz się, w jaki sposób wygenerować losowe liczby w języku Swift.

## Jak to zrobić

Do generowania losowych liczb w Swift wykorzystujemy klasę `Int` oraz funkcję `random(in:)`. Przykładowy kod może wyglądać następująco:

```Swift
// Wygenerowanie losowej liczby całkowitej z zakresu od 0 do 100
let randomNumber = Int.random(in: 0...100)
print(randomNumber)
// Wynik: np. 67
```

Możemy również wygenerować losową liczbę zmiennoprzecinkową w podanym zakresie za pomocą funkcji `Double.random(in:)` lub `Float.random(in:)`.

```Swift
// Wygenerowanie losowej liczby zmiennoprzecinkowej z zakresu od 0 do 1
let randomNumber = Double.random(in: 0...1)
print(randomNumber)
// Wynik: np. 0.732977
```

Możliwe jest także wygenerowanie losowych wartości logicznych (true lub false) za pomocą funkcji `Bool.random()`.

```Swift
// Wygenerowanie losowej wartości logicznej - true lub false
let randomBoolean = Bool.random()
print(randomBoolean)
// Wynik: np. true
```

## Głębszy przegląd

Generator losowych liczb w języku Swift wykorzystuje algorytm Mersenne Twister, który jest szeroko używany w różnych językach programowania. Dzięki temu możemy być pewni, że wygenerowane przez nas liczby będą w jak największym stopniu losowe.

## Zobacz także
- Dokumentacja funkcji `random(in:)`: https://developer.apple.com/documentation/swift/int/2994691-random
- Wykorzystanie generatora liczb losowych w aplikacjach iOS: https://www.tutorialspoint.com/generating-random-numbers-in-ios-using-swift
- Wprowadzenie do Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister