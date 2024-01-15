---
title:                "Generowanie losowych liczb"
html_title:           "TypeScript: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest ważnym aspektem wielu programów i aplikacji. Może być wykorzystywane do tworzenia losowych danych do testów lub gier, losowego wybierania opcji czy generowania unikatowych identyfikatorów. W tym artykule dowiesz się, jak w prosty sposób wygenerować losowe liczby w języku TypeScript.

## Jak to zrobić

```typescript
// Tworzenie funkcji generującej losową liczbę całkowitą
function randomInt(min: number, max: number) {
    // Generowanie liczby losowej
    return Math.floor(Math.random() * (max - min + 1) + min);
}

// Użycie funkcji
console.log(randomInt(1, 10)); // Output: losowa liczba całkowita z przedziału 1-10
```

```typescript
// Tworzenie funkcji generującej losową liczbę zmiennoprzecinkową
function randomFloat(min: number, max: number) {
    // Generowanie liczby losowej
    return Math.random() * (max - min) + min;
}

// Użycie funkcji
console.log(randomFloat(1, 10)); // Output: losowa liczba zmiennoprzecinkowa z przedziału 1-10
```

## Deep Dive

Generowanie liczb losowych może być wykonane za pomocą wbudowanej metody `Math.random()`, która zwraca wartość z przedziału od 0 (włącznie) do 1 (bez włączenia). Aby uzyskać liczbę z określonego przedziału, można wykorzystać prosty wzór, jak pokazane w przykładach powyżej. Warto zauważyć, że generowanie liczb losowych jest wysoce pseudolosowe, ponieważ podstawowym algorytmem jest zwykle generator liczb pseudolosowych (PRNG), który jest deterministyczny i zwraca sekwencję liczb, która wydaje się być losowa, ale w rzeczywistości jest wygenerowana na podstawie ustalonego początkowego stanu.

## Zobacz też

- [Dokumentacja języka TypeScript](https://www.typescriptlang.org/docs/)
- [Dokumentacja dla obiektu Math w języku JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Math)