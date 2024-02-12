---
title:                "Drukowanie komunikatów debugowania"
aliases:
- /pl/typescript/printing-debug-output/
date:                  2024-01-20T17:53:28.788973-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wypisywanie informacji debugujących to wyświetlanie danych, które pomagają programiście zrozumieć, co się dzieje w kodzie. Programiści robią to, by szybko znaleźć i naprawić błędy.

## How to: (Jak to zrobić?)
W TypeScript możesz wykorzystać `console.log` do pokazywania debug info. Spójrz jak to działa:

```TypeScript
function addNumbers(a: number, b: number): number {
    console.log(`Adding ${a} + ${b}`);
    return a + b;
}

const sum = addNumbers(2, 3);
console.log(`Sum: ${sum}`);
```

Output będzie wyglądać tak:
```
Adding 2 + 3
Sum: 5
```

Możesz też użyć `console.error` dla błędów i `console.warn` dla ostrzeżeń.

```TypeScript
function subtractNumbers(a: number, b: number): number {
    if (b > a) {
        console.warn('Subtracting larger number from smaller number.');
    }
    return a - b;
}

const result = subtractNumbers(5, 10);
console.error(`Result: ${result}`);
```

W ten sposób otrzymujemy:
```
Warning: Subtracting larger number from smaller number.
Error: Result: -5
```

## Deep Dive (Dogłębna analiza)
Debugowanie kodu przez wypisywanie ma swoje korzenie w początkach informatyki, gdzie logi były sprawdzane na papierze. Dziś mamy wiele narzędzi, ale `console.log` wciąż jest używany przez swe prostotę i uniwersalność.

Alternatywy dla `console.log` w TypeScript/JavaScript to: debugger oraz zaawansowane systemy logowania jak Winston czy Bunyan, które oferują filtrowanie i lepsze zarządzanie logami.

Szczegółowo, `console` w Node.js i przeglądarkach może mieć różne implementacje, choć API jest standardowe. Dlatego czasami logi mogą różnić się między środowiskami.

## See Also (Zobacz również)
- MDN Web Docs na temat `Console`: https://developer.mozilla.org/en-US/docs/Web/API/Console
- Node.js dokumentacje dla `console`: https://nodejs.org/api/console.html
- Porównanie bibliotek do logowania: https://stackify.com/best-javascript-logging-libraries/
