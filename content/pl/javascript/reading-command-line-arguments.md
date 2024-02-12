---
title:                "Odczytywanie argumentów linii poleceń"
aliases:
- pl/javascript/reading-command-line-arguments.md
date:                  2024-01-20T17:56:16.044210-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

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
