---
date: 2024-01-20 17:57:18.727246-07:00
description: "How to (Jak to zrobi\u0107): ."
lastmod: '2024-03-13T22:44:35.155141-06:00'
model: gpt-4-1106-preview
summary: .
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## How to (Jak to zrobić):
```TypeScript
// Instalujemy Node.js i TypeScript, a potem tworzymy plik index.ts

// Używamy process.argv do zbierania argumentów
const args = process.argv.slice(2); // Usuwamy pierwsze dwa argumenty ('node' i ścieżka do skryptu)

// Pokazujemy argumenty
console.log(args);

// Uruchamiamy skrypt z argumentami:
// tsc index.ts && node index.js arg1 arg2

// Przykładowe wyjście:
// ['arg1', 'arg2']
```

## Deep Dive (Dogłębna analiza):
Czytanie argumentów linii poleceń to funkcjonalność widoczna w wielu językach, często od ich zarania. W Node.js `process.argv` to standardowy sposób na dostęp do nich. Alternatywy jak np. biblioteki `yargs` czy `commander` zapewniają więcej opcji i łatwiejszą syntaktykę.

Szczegóły implementacyjne Node.js - globalny obiekt `process` jest instancją `EventEmitter` i udostępnia informacje o aktualnie działającym procesie. Właśnie z jego `argv` (argument vector), program zbiera surowe argumenty przekazane do procesu.

## See Also (Zobacz również):
- Node.js documentation on `process.argv`: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- `yargs` library: https://www.npmjs.com/package/yargs
- `commander` library: https://www.npmjs.com/package/commander
