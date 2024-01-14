---
title:                "TypeScript: Odczytywanie argumentów wiersza poleceń"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w języku TypeScript może być różnorodne i wymagać wielu umiejętności, ale jednym z podstawowych elementów jest umiejętność czytania argumentów wiersza poleceń. Niektórzy programiści mogą uważać to za zbędne lub trudne, ale prawda jest taka, że ​​umiejętność czytania argumentów wiersza poleceń może znacznie ułatwić pracę z programami w trybie konsolowym.

## Jak to zrobić

Aby czytać argumenty wiersza poleceń w języku TypeScript, należy użyć wbudowanego modułu "process". Ten moduł umożliwia dostęp do wszystkich argumentów przekazanych podczas uruchamiania programu. Poniżej przedstawiono przykładowy kod, który przedstawia, jak odczytać argumenty wiersza poleceń i wyświetlić je w konsoli.

```TypeScript
const args = process.argv.slice(2);
console.log(args);
```

W tym przykładzie zmienna "args" zawiera tablicę wszystkich argumentów przekazanych wierszu poleceń. Dzięki temu możemy łatwo operować na przekazywanych argumentach i wykorzystać je w naszym programie. Poniżej znajduje się przykładowy wynik wyświetlony w konsoli.

```
node index.ts argument1 argument2 argument3
```

Wyjście:
```
[ 'argument1', 'argument2', 'argument3']
```

## Deep Dive

Warto zauważyć, że pierwszy element tablicy "process.argv" zawsze będzie zawierał ścieżkę do aplikacji Node.js, a drugi element będzie zawierał ścieżkę do uruchomionego pliku. Dzięki temu nasze działania mogą być dostosowane do różnych środowisk uruchomieniowych.

Ponadto, do manipulacji argumentami wiersza poleceń możemy wykorzystać również wbudowany moduł "yargs". Ten moduł wykorzystuje strukturę linii poleceń i umożliwia łatwiejsze i bardziej elastyczne parsowanie argumentów. Przykładowy kod wykorzystujący moduł "yargs" mogłby wyglądać tak:

```TypeScript
import yargs from 'yargs';

const args = yargs(process.argv.slice(2)).argv;
console.log(args);
```

W ten sposób możemy dodatkowo przypisać wartości i opcje do poszczególnych argumentów, co czyni nasz kod bardziej przejrzystym i dostępnych dla innych deweloperów.

## Zobacz również

- [Dokumentacja wbudowanego modułu "process"](https://nodejs.org/api/process.html)
- [Dokumentacja modułu "yargs"](https://www.npmjs.com/package/yargs)