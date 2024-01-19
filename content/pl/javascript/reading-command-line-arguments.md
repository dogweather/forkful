---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytając argumenty linii poleceń, możemy wpływać na działanie naszych programów bez konieczności modyfikowania kodu źródłowego. Programiści korzystają z tej techniki, aby utworzyć skrypty bardziej elastyczne i łatwiejsze w konfiguracji.

## Jak to zrobić:

W Node.js, możesz uzyskać argumenty linii poleceń z właściwości `process.argv`. Oto przykład:

```Javascript
console.log(process.argv)
```

Gdy uruchomisz powyższy skrypt z argumentami, tzn:

```Javascript
node myScript.js arg1 arg2
```

Owoce Twojej pracy będą wyglądać tak:

```Javascript
[ 'C:\\Program Files\\nodejs\\node.exe',
  'C:\\Users\\YourName\\myScript.js',
  'arg1',
  'arg2' ]
```

Pierwsze dwa elementy to domyślne argumenty. Faktyczne argumenty zaczynają się od trzeciego elementu.

## Deep Dive:

Język Javascript nie był pierwotnie zaprojektowany do odczytywania argumentów z linii poleceń. Dopiero z pojawieniem się Node.js, argumenty z linii poleceń stały się dostępne poprzez obiekt `process`.

Jest wiele alternatywnych bibliotek do obsługi argumentów linii poleceń, jak `commander.js` czy `yargs`, które oferują więcej funkcji i łatwiejsze w użyciu interfejsy.

W przeglądarce, nie jest możliwe bezpośrednie odczytanie argumentów z linii poleceń. Przeważnie dane wejściowe są zbierane przez interfejs użytkownika lub zasoby sieciowe.

## Zobacz także:

- Node.js process.argv: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Commander.js: https://github.com/tj/commander.js/
- Yargs: https://yargs.js.org/