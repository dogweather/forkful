---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:50:18.551434-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Generowanie liczb losowych to tworzenie pozornie nieprzewidywalnych numerów. Programiści używają tego, by wspomóc symulacje, testy czy gry - wszędzie tam, gdzie potrzebna jest nieprzewidywalność.

## How to (Jak to zrobić):
```TypeScript
// Proste generowanie liczby losowej od 0 do 1
console.log(Math.random());

// Losowa liczba całkowita między min a max
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Przykład użycia
console.log(getRandomInt(1, 10));
```
Sample output (Przykładowe wyjście):
```
0.4356941481639862
7
```

## Deep Dive (Dogłębna analiza):
Historia generatorów liczb losowych (RNG) sięga starożytnych czasów, ale komputery zmieniły zasady gry. W informatyce na ogół używa się generatorów pseudo-losowych (PRNG), bo rzeczywista losowość jest bardzo trudna do osiągnięcia.

Algorytm `Math.random()` w JavaScript (używany również w TypeScript) jest PRNG. Nie jest odpowiedni dla aplikacji kryptograficznych, bo jego wyniki można przewidzieć. Do takich celów lepiej użyć `crypto.getRandomValues()`. 

Każdy PRNG ma tzw. "ziarno" (seed), będące punktem startowym dla generowania liczb. Dwa identyczne ziarna wygenerują ten sam ciąg liczb. Praktyczność tego mechanizmu jest różna - od prostych gier po zaawansowane symulacje.

## See Also (Zobacz również):
- MDN Web Docs on `Math.random()`: [MDN Math.random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- The TypeScript Handbook: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- "Randomness in crypto with TypeScript": [Crypto randomness](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
