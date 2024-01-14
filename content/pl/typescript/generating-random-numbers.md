---
title:                "TypeScript: Tworzenie losowych liczb"
simple_title:         "Tworzenie losowych liczb"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb może być bardzo przydatne w programowaniu. Może to pomóc w tworzeniu losowych symulacji, losowego wybierania elementów, generowania unikalnych identyfikatorów i wielu innych zastosowań.

## Jak To Zrobić

Aby wygenerować losową liczbę w TypeScript, możesz użyć funkcji `Math.random()`, która zwraca liczbę zmiennoprzecinkową między 0 a 1. Aby uzyskać większą liczbę, możesz pomnożyć ją przez maksymalną wartość, którą chcesz uzyskać. Na przykład, jeśli chcesz uzyskać losową liczbę między 1 a 10, możesz pomnożyć wynik przez 10 i dodać 1, co wygląda następująco:

```TypeScript
let randomNumber: number = Math.random() * 10 + 1;
console.log(randomNumber); // output: 7.23958242
```

Możesz również użyć funkcji `Math.floor()`, aby zaokrąglić liczby w dół i otrzymać całkowitą liczbę. Na przykład, jeśli chcesz wygenerować losową liczbę między 1 a 10, możesz użyć funkcji `Math.floor()` w następujący sposób:

```TypeScript
let randomNumber: number = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); // output: 9
```

Możesz również zastosować te techniki do generowania losowego indeksu lub elementu z tablicy. Na przykład, jeśli masz tablicę z imionami i chcesz wylosować jedno z nich, możesz użyć funkcji `Math.floor()` w następujący sposób:

```TypeScript
let names: string[] = ["Anna", "Jan", "Kasia", "Marek"];
let randomIndex: number = Math.floor(Math.random() * names.length);
let randomName: string = names[randomIndex];
console.log(randomName); // output: Marek
```

## Głębsza Analiza

Funkcja `Math.random()` wykorzystuje algorytm pseudolosowy do generowania liczb. Oznacza to, że wygenerowane liczby nie są prawdziwie losowe, ale są wystarczająco losowe dla większości zastosowań programistycznych. Jeśli potrzebujesz większej losowości, możesz użyć zewnętrznych bibliotek lub uruchomić funkcję `Math.random()` wiele razy i mieszać wyniki.

## Zobacz Również

- Dokumentacja Math.random() w przewodniku po języku TypeScript (https://www.typescriptlang.org/docs/handbook/numbers.html#random)
- Generowanie losowych liczb z TypeScript w 5 prostych krokach (https://ninjalearn.com/typescript-random-number/)
- Zastosowania funkcji random w programowaniu (https://www.geeksforgeeks.org/generating-random-number-infinity-using-javascript-math-random-function/)