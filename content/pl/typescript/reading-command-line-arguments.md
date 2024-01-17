---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "TypeScript: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czy kiedykolwiek słyszałeś o czytaniu argumentów z wiersza poleceń? To po prostu oznacza odczytywanie danych wprowadzonych przez użytkownika w terminalu lub wierszu poleceń na komputerze. Programiści wykonują to aby móc dostosować wykonywane polecenia lub dostosować swoje aplikacje do różnych przypadków użytkowania.

## Jak to zrobić:
Poniżej przedstawiono przykład kodu w języku TypeScript, który odczytuje argumenty z wiersza poleceń:
```TypeScript
import * as process from 'process';

let args: string[] = process.argv;
for (let i: number = 0; i < args.length; i++) {
    console.log(`Argument ${i + 1}: ${args[i]}`);
}
```
Wywołując ten kod z wiersza poleceń przy użyciu wiersza poleceń TypeScript: node index.ts argument1 argument2
Otrzymamy następujący wynik:
```
Argument 1: node
Argument 2: index.ts
Argument 3: argument1
Argument 4: argument2
```

## Pełne zanurzenie:
Czytanie argumentów z wiersza poleceń jest bardzo przydatne i często wykorzystywane w programowaniu. Historia sięga lat 60., kiedy to pojawiły się pierwsze systemy operacyjne z wierszem poleceń. Alternatywnym sposobem na odczytanie argumentów jest korzystanie z interfejsu użytkownika lub plików konfiguracyjnych, jednak nie są one tak wygodne w użyciu jak wiersz poleceń.
Implementacja odczytywania argumentów z wiersza poleceń jest wykonywana przy użyciu funkcji dostępnych w bibliotece process w języku TypeScript.

## Zobacz również:
- [Dokumentacja process w języku TypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Informacje o wierszu poleceń](https://en.wikipedia.org/wiki/Command-line_interface)