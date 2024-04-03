---
date: 2024-01-20 17:56:16.044210-07:00
description: "How to: U\u017Cyj `process.argv`, \u017Ceby dosta\u0107 si\u0119 do\
  \ argument\xF3w. Pierwsze dwa argumenty to \u015Bcie\u017Cka do \u015Brodowiska\
  \ Node i pliku, kt\xF3ry wykonujesz, wi\u0119c prawdziwe\u2026"
lastmod: '2024-03-13T22:44:35.811798-06:00'
model: gpt-4-1106-preview
summary: "U\u017Cyj `process.argv`, \u017Ceby dosta\u0107 si\u0119 do argument\xF3\
  w."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

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
