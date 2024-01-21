---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:51:40.464485-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolacja to wplecenie zmiennej w ciąg tekstowy, co ułatwia tworzenie dynamicznego tekstu. Programiści używają interpolacji, aby łatwo łączyć stałe fragmenty tekstu z zmiennymi danymi, co czyni kod bardziej czytelnym i elastycznym.

## How to:
```TypeScript
let user = 'Jan Kowalski';
let age = 28;

// Standardowy sposób interpolacji w TypeScript:
let greeting = `Cześć, mam na imię ${user} i mam ${age} lata.`;

console.log(greeting); // Wydruk: Cześć, mam na imię Jan Kowalski i mam 28 lata.
```

## Deep Dive
Historia interpolacji stringów sięga języków programowania, które używały operatorów konkatenacji do łączenia ciągów znaków. W nowoczesnym JavaScript i TypeScript, template strings (ciągi szablonowe) ułatwiły ten proces dzięki użyciu backticków (``) oraz zapisu `${expression}`.

Oprócz czytelności, interpolacja umożliwia wstawianie wyników wywołań funkcji czy obliczeń bezpośrednio w string. Jest wydajniejsza niż klasyczna konkatenacja, gdyż nie tworzy wielu tymczasowych stringów w pamięci.

Alternatywy to konkatenacja przy pomocy `+` i funkcja `concat()`, ale są mniej wygodne i intuicyjne.

Implementacja w TypeScript jest prawie identyczna jak w ES6 (ECMAScript 2015), ponieważ TypeScript jest supersetem JavaScript.

## See Also
- MDN Web Docs na temat template strings: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- Dokumentacja TypeScript – String: https://www.typescriptlang.org/docs/handbook/basic-types.html#string
- TypeScript Deep Dive o stringach i interpolacji: https://basarat.gitbook.io/typescript/type-system#string