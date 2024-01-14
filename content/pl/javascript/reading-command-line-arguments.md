---
title:    "Javascript: Odczytywanie argumentów wiersza poleceń"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w języku Javascript jest bardzo popularne, a znajomość tej umiejętności może otworzyć przed nami wiele możliwości zawodowych. Jednym z ważnych aspektów programowania jest umiejętność czytania argumentów linii poleceń. Pozwala to na dostosowanie zachowania programu w zależności od wprowadzonych parametrów. W tym artykule dowiecie się jak to zrobić!

## Jak to zrobić

Czytanie argumentów linii poleceń w języku Javascript jest bardzo proste. Wystarczy wykorzystać wbudowane obiekty process.argv, które przechowują wprowadzone przez użytkownika parametry. Poniżej znajduje się przykładowy kod pokazujący jak wygląda to w praktyce:
```Javascript
const args = process.argv;

console.log("Pierwszy argument: ", args[2]);
console.log("Drugi argument: ", args[3]);
```

Przykładowe wywołanie: `node program.js argument1 argument2`

Przykładowy output:
```
Pierwszy argument: argument1
Drugi argument: argument2
```
Jak widać, argumenty linii poleceń są przechowywane w tablicy args, a dostęp do nich odbywa się przez indeksy. Pamiętajcie, że pierwsze dwa argumenty to ścieżka do pliku i nazwa programu, dlatego najpierw pobieramy wartość args[2].

## Deep Dive

Teraz przejdźmy do większego wyzwania - jak czytać argumenty linii poleceń w bardziej złożonym przypadku, gdzie mamy wiele parametrów. W takiej sytuacji przydatne może być wykorzystanie biblioteki yargs, która ułatwia parsowanie argumentów linii poleceń. Poniżej znajduje się przykładowy kod, który pokazuje jak to zrobić:
```Javascript
const yargs = require('yargs');

const argv = yargs
    .options({
        'first': {
            alias: 'f',
            demandOption: true,
            describe: "Pierwszy argument",
            type: 'string'
        },
        'second': {
            alias: 's',
            demandOption: true,
            describe: "Drugi argument",
            type: 'string'
        }
    })
    .argv;

console.log("Pierwszy argument: ", argv.first);
console.log("Drugi argument: ", argv.second);

```

Przykładowe wywołanie: `node program.js -f argument1 -s argument2`

Przykładowy output:
```
Pierwszy argument: argument1
Drugi argument: argument2
```

Warto zwrócić uwagę na to, że biblioteka yargs pozwala na bardziej zaawansowane konfiguracje, jak na przykład wykorzystanie aliasów, wymuszenie obecności parametru czy też sprawdzenie jego typu.

## Zobacz także

- Dokumentacja wbudowanego obiektu process w języku Javascript: https://nodejs.org/dist/latest-v14.x/docs/api/process.html
- Oficjalna strona biblioteki yargs: https://yargs.js.org/