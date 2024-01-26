---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:25.117086-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Generowanie losowych numerów to proces tworzenia nieprzewidywalnych liczb. Programiści używają go do wszystkiego, od gier i symulacji po bezpieczeństwo i analizy statystyczne.

## Jak to zrobić:
JavaScript oferuje wbudowaną funkcję `Math.random()` do tworzenia liczb losowych z przedziału od 0 (włącznie) do 1 (wyłączając). Chcesz coś bardziej konkret? Oto kilka szybkich przykładowych skryptów:

```Javascript
// Prosty losowy numer od 0 do 1
console.log(Math.random());

// Losowy numer od 0 do 10
console.log(Math.random() * 10);

// Losowy numer całkowity od 0 do 10
console.log(Math.floor(Math.random() * 11));

// Losowy numer całkowity od 1 do 10
console.log(Math.floor(Math.random() * 10) + 1);

// Funkcja do generowania losowych numerów pomiędzy min a max
function getRandomBetween(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Użycie funkcji
console.log(getRandomBetween(5, 15));
```

Próbki wyników:
```
0.437829...
4.927362...
6
9
12 (przykład z przedziału 5 do 15)
```

## Głębsze spojrzenie:
Losowość w JavaScript używa algorytmu pseudolosowego - dość dobrego, ale nie idealnego dla zaawansowanego kryptograficznego bezpieczeństwa. W latach 90. algorytmy jak LCG (Linear Congruential Generator) były popularne, a teraz mamy bardziej skomplikowane metody jak Mersenne Twister, ale `Math.random()` nie ujawnia, którego używa.

Jeśli potrzebujesz kryptograficznie bezpiecznych liczb losowych, użyj Web Crypto API. Oto jak:

```Javascript
window.crypto.getRandomValues(new Uint32Array(1))[0];
```

Chcesz symulować rzut kością? Używaj funkcji z przykładu powyżej. Generowanie liczb losowych w grach, symulacjach czy do zbierania próbek danych to standard. Zrozumienie, jak to się dzieje pod maską, daje lepszą kontrolę i wiarygodność wyników.

## Zobacz też:
- MDN Web Docs na temat `Math.random()`: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- MDN Web Docs na temat Web Crypto API: [https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API)
- Artykuł o algorytmach generowania liczb losowych: [https://en.wikipedia.org/wiki/List_of_random_number_generators](https://en.wikipedia.org/wiki/List_of_random_number_generators)
- Interaktywny kurs o losowości i matematyce: [https://www.khanacademy.org/math/statistics-probability](https://www.khanacademy.org/math/statistics-probability)
