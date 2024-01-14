---
title:                "Javascript: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest ważnym elementem w wielu aplikacjach i programach komputerowych. Może być wykorzystywane do symulacji losowych zdarzeń, gier lub do testowania algorytmów. Jest to także przydatne narzędzie w statystyce i analizie danych. W tym wpisie dowiesz się, jak w prosty sposób możesz generować losowe liczby w języku Javascript.

## Jak to zrobić

Aby wygenerować losową liczbę w zakresie od 0 do 10, należy użyć metody `Math.random()` w połączeniu z mnożeniem przez 10 i zaokrąglenie za pomocą `Math.floor()`:

```Javascript
let randomNum = Math.floor(Math.random() * 10);
console.log(randomNum); // Przykładowy wynik: 7
```

Aby wygenerować liczbę w określonym zakresie, należy określić dolny i górny limit oraz dodać wartość początkową do wyniku: 

```Javascript
let lowerLimit = 5;
let upperLimit = 10;
let randomRange = Math.floor(Math.random() * (upperLimit - lowerLimit + 1)) + lowerLimit;
console.log(randomRange); // Przykładowy wynik: od 5 do 10
```

Możesz także wygenerować losowe liczby zmiennoprzecinkowe, dodając ułamek do metody `Math.random()`:

```Javascript
let randomDecimal = Math.random() + 5;
console.log(randomDecimal); // Przykładowy wynik: od 5 (włącznie) do 6 (bez 6)
```

## Deep Dive

W języku Javascript, metoda `Math.random()` zwraca wartości z przedziału od 0 (włącznie) do 1 (bez 1). Dzięki temu, możesz dostosować zakres według własnych potrzeb za pomocą mnożenia, dodawania lub odejmowania.

Istnieje także możliwość zdefiniowania własnej funkcji do generowania liczb losowych, aby dostosować zakres lub sposób generowania. Poniższy przykład wykorzystuje funkcję `randomRange()` z podanymi parametrami dolnego i górnego limitu:

```Javascript
function randomRange(lowerLimit, upperLimit) {
  return Math.floor(Math.random() * (upperLimit - lowerLimit + 1)) + lowerLimit;
}

console.log(randomRange(1, 5)); // Przykładowy wynik: od 1 do 5
console.log(randomRange(50, 100)); // Przykładowy wynik: od 50 do 100
```

## Zobacz również

- [Metoda Math.random() w dokumentacji MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Math/random)
- [Przykłady użycia Math.random() na stronie W3schools](https://www.w3schools.com/js/js_random.asp)