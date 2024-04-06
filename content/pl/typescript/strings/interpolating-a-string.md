---
date: 2024-01-20 17:51:40.464485-07:00
description: "How to: Historia interpolacji string\xF3w si\u0119ga j\u0119zyk\xF3\
  w programowania, kt\xF3re u\u017Cywa\u0142y operator\xF3w konkatenacji do \u0142\
  \u0105czenia ci\u0105g\xF3w znak\xF3w. W nowoczesnym\u2026"
lastmod: '2024-04-05T21:53:36.565356-06:00'
model: gpt-4-1106-preview
summary: "Historia interpolacji string\xF3w si\u0119ga j\u0119zyk\xF3w programowania,\
  \ kt\xF3re u\u017Cywa\u0142y operator\xF3w konkatenacji do \u0142\u0105czenia ci\u0105\
  g\xF3w znak\xF3w."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

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
