---
date: 2024-01-20 18:04:48.263773-07:00
description: "How to: (Jak to zrobi\u0107:) TypeScript pojawi\u0142 si\u0119 w 2012\
  \ roku jako nadzbi\xF3r JavaScript, dodaj\u0105c typowanie statyczne do j\u0119\
  zyka. Alternatyw\u0105 mo\u017Ce by\u0107 Flow od\u2026"
lastmod: '2024-04-05T22:50:49.442958-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) TypeScript pojawi\u0142 si\u0119 w 2012 roku jako\
  \ nadzbi\xF3r JavaScript, dodaj\u0105c typowanie statyczne do j\u0119zyka."
title: Rozpoczynanie nowego projektu
weight: 1
---

## How to: (Jak to zrobić:)
```TypeScript
// Instalacja TypeScript globalnie
npm install -g typescript

// Inicjowanie nowego projektu Node.js
npm init -y

// Tworzenie pliku tsconfig.json
tsc --init

// Przykładowy plik hello.ts
function sayHello(name: string): void {
  console.log(`Hello, ${name}!`);
}

sayHello('World');

// Kompilacja do JavaScript i uruchomienie
tsc hello.ts
node hello.js
```
Wynik:
```
Hello, World!
```

## Deep Dive (Dogłębna analiza)
TypeScript pojawił się w 2012 roku jako nadzbiór JavaScript, dodając typowanie statyczne do języka. Alternatywą może być Flow od Facebooka lub Elm, bardziej funkcyjny i bezpieczny dla aplikacji front-endowych. Implementacja nowego projektu w TypeScript wymaga konfiguracji pliku tsconfig.json, który definiuje opcje kompilacji, środowisko wykonawcze i inne preferencje projektu. TypeScript umożliwia efektywniejsze zarządzanie dużymi projektami dzięki lepszej organizacji kodu i zapewnieniu typów, które ułatwiają wykrywanie błędów na etapie kompilacji.

## See Also (Zobacz również)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [Node.js Documentation](https://nodejs.org/en/docs/)
- [npm Documentation](https://docs.npmjs.com/)
- [Understanding tsconfig.json](https://www.typescriptlang.org/tsconfig)
