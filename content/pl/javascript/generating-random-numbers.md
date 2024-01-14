---
title:                "Javascript: Generowanie losowych liczb"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego zainteresować się generowaniem losowych liczb?

Istnieje wiele różnych zastosowań dla generowania losowych liczb w Javascript. Można je wykorzystać do tworzenia gier, testowania kodu, lub nawet tworzenia unikatowych identyfikatorów dla użytkowników. Generowanie losowych liczb pozwala programistom na wprowadzanie elementu losowości do swoich projektów, co może być bardzo przydatne w wielu przypadkach.

## Jak to zrobić?

Generowanie losowych liczb w Javascript jest bardzo proste dzięki wbudowanej funkcji `Math.random()`. Ta funkcja zwraca liczbę z przedziału od 0 do 1 (włącznie). Aby wygenerować liczbę z innego przedziału, należy odpowiednio przemnożyć i dodać wynik funkcji `Math.random()`.

```Javascript
// Generowanie losowego liczbę z przedziału od 0 do 10
let randomNum = Math.random() * 10;

// Generowanie losowej liczby z przedziału od 50 do 100
let randomNum = Math.random() * (100 - 50 + 1) + 50; 
```

Można również wykorzystać funkcję `Math.floor()` do zaokrąglenia wygenerowanej liczby w dół do najbliższej całkowitej wartości.

```Javascript
// Generowanie losowej liczby całkowitej z przedziału od 1 do 10
let randomNum = Math.floor(Math.random() * 10) + 1;
```

Pamiętaj, że funkcja `Math.random()` zwraca liczbę pseudolosową, a nie całkowicie losową. Oznacza to, że można przewidzieć jej wartość, ale tylko w pewnym stopniu. Dlatego też nie należy polegać na losowości generowanych liczb w zastosowaniach, które wymagają bezpieczeństwa lub niezawodności.

## Głębsze spojrzenie na generowanie losowych liczb

Podczas korzystania z funkcji `Math.random()` ważne jest, aby pamiętać, że wartość zwracana jest zawsze liczbą typu `Number`. To oznacza, że można wykorzystać ją w niektórych matematycznych operacjach, takich jak dodawanie i odejmowanie. Można również wykorzystać ją do generowania losowych indeksów do tablic lub losowego wybierania elementów z tablicy.

Można również utworzyć własną funkcję do generowania losowych liczb w określonym przedziale.

```Javascript
// Funkcja do generowania losowej liczby z danego przedziału
function getRandomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Przykładowe użycie funkcji dla przedziału od 1 do 10
let randomNum = getRandomNumber(1, 10);
```

## Zobacz także

- [Dokumentacja Math.random() w języku angielskim](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Wprowadzenie do generowania losowych liczb w Javascript (po angielsku)](https://www.tutorialspoint.com/generate-random-number-in-javascript)