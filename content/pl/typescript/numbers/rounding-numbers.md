---
title:                "Zaokrąglanie liczb"
aliases:
- /pl/typescript/rounding-numbers/
date:                  2024-01-26T03:47:11.275604-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb to redukowanie liczby do określonej precyzji. Programiści robią to, aby kontrolować numeryczne wyjście dla czytelności, celów wyświetlania, lub kiedy wymagana jest określona precyzja po operacjach, które dają wyniki zmiennoprzecinkowe.

## Jak to zrobić:
Zaokrąglanie w TypeScript można wykonać przy użyciu kilku metod. Oto szybki przegląd:

```typescript
// Math.round zaokrągla do najbliższej liczby całkowitej
console.log(Math.round(1.5)); // Wynik: 2

// Math.ceil zaokrągla w górę do najbliższej liczby całkowitej
console.log(Math.ceil(1.1)); // Wynik: 2

// Math.floor zaokrągla w dół do najbliższej liczby całkowitej
console.log(Math.floor(1.8)); // Wynik: 1

// toFixed zaokrągla do ustalonej liczby miejsc dziesiętnych
let num = 1.23456;
console.log(num.toFixed(2)); // Wynik: "1.23"
// Uwaga: toFixed zwraca łańcuch znaków! Użyj parseFloat, aby w razie potrzeby przekonwertować z powrotem.
console.log(parseFloat(num.toFixed(2))); // Wynik: 1.23
```

## Wnikliwe spojrzenie
W dawnych czasach zaokrąglanie było koniecznością z powodu ograniczonej przestrzeni i problemów z precyzją we wczesnych komputerach. Dzisiaj, arytmetyka zmiennoprzecinkowa może prowadzić do dziwnych wyników ze względu na sposób przechowywania liczb w systemie binarnym. Alternatywy dla zaokrąglania obejmują floor, ceil i trunc (do obcinania miejsc dziesiętnych bez zaokrąglania).

Detale mają znaczenie: `Math.round` stosuje "zaokrąglanie pół do góry" (znane również jako "zaokrąglanie handlowe"), podczas gdy `Math.floor` i `Math.ceil` są proste w działaniu. `toFixed` może powodować nieoczekiwane wyniki, ponieważ zwraca łańcuch znaków, i zaokrągla używając "zaokrąglenia pół do parzystej" (znane również jako "zaokrąglanie bankowe"), szczególnie przydatne do redukcji stronniczości przy wielokrotnym zaokrąglaniu tych samych liczb.

## Zobacz również
- [MDN - Math.round()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE Standard dla arytmetyki zmiennoprzecinkowej (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
