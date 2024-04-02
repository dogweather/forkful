---
date: 2024-01-20 17:56:16.044210-07:00
description: "Czytanie argument\xF3w linii polece\u0144 to sposob odbierania danych\
  \ z zewn\u0105trz przez twoj\u0105 aplikacj\u0119 Node.js. Robimy to, \u017Ceby\
  \ elastycznie manipulowa\u0107\u2026"
lastmod: '2024-03-13T22:44:35.811798-06:00'
model: gpt-4-1106-preview
summary: "Czytanie argument\xF3w linii polece\u0144 to sposob odbierania danych z\
  \ zewn\u0105trz przez twoj\u0105 aplikacj\u0119 Node.js. Robimy to, \u017Ceby elastycznie\
  \ manipulowa\u0107\u2026"
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## What & Why?
Czytanie argumentów linii poleceń to sposob odbierania danych z zewnątrz przez twoją aplikację Node.js. Robimy to, żeby elastycznie manipulować zachowaniem programu bez potrzeby zmiany kodu.

## How to:
Użyj `process.argv`, żeby dostać się do argumentów. Pierwsze dwa argumenty to ścieżka do środowiska Node i pliku, który wykonujesz, więc prawdziwe argumenty startują z indexu 2.

```javascript
// myscript.js
console.log(process.argv);

// Uruchomienie w terminalu:
// node myscript.js arg1 arg2 arg3

/*
Output:
[
  '/path/to/node',
  '/path/to/your/script/myscript.js',
  'arg1',
  'arg2',
  'arg3'
]
*/
```
Istnieją też biblioteki jak `yargs` lub `commander`, które upraszczają ten proces i dodają użyteczne funkcje.

## Deep Dive
Historia argumentów linii poleceń sięga początków interfejsów tekstowych - to podstawowy sposób interakcji z systemami UNIX. Wiele języków (C, Python, Node.js) używa podobnych metod do ich przetwarzania. Alternatywą może być użycie plików konfiguracyjnych lub zmiennych środowiskowych, ale te metody są mniej dynamiczne. W Node.js, `process.argv` jest prostym, acz potężnym rozwiązaniem, które można jeszcze ulepszyć przez zewnętrzne pakiety.

## See Also
- [Node.js process.argv documentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Yargs package](https://www.npmjs.com/package/yargs)
- [Commander package](https://www.npmjs.com/package/commander)
