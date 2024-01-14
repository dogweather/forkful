---
title:    "TypeScript: Odczytywanie argumentów linii poleceń."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek pracowałeś z aplikacjami, które muszą otrzymywać dane od użytkownika za każdym razem, gdy są uruchamiane? Jeśli tak, może zastanawiałeś się, jak można to zrobić w TypeScript. W tym krótkim poradniku dowiesz się, jak czytać argumenty wiersza poleceń przy użyciu tego języka programowania i dlaczego jest to ważne.

## Jak to zrobić

Aby odczytać argumenty wiersza poleceń w TypeScript, musisz zaimportować moduł "process" i wykorzystać jego metodę "argv". Oto przykładowy fragment kodu:

```TypeScript
import * as process from 'process';

const args = process.argv;
console.log(args);
```

Powyższy kod spowoduje wyświetlenie tablicy z wszystkimi argumentami wiersza poleceń, które zostały przekazane do aplikacji. Na przykład, jeśli uruchomisz ten kod z argumentami "node index.ts --name Jan", wyświetli się następująca tablica:

```
["node", "index.ts", "--name", "Jan"]
```

Możesz również ustawić domyślną wartość dla argumentów, jeśli nie zostaną one przekazane. W tym celu możesz użyć metody "slice", która pozwala pominąć pierwsze dwa elementy tablicy z argumentami ("node" i "index.ts"). Poniżej znajduje się przykład:

```TypeScript
import * as process from 'process';

const args = process.argv.slice(2);
const name = args[0] || 'Anonymous';
console.log(`Witaj, ${name}!`);
```

Jeśli uruchomisz ten kod bez podania argumentu, zostanie wyświetlone "Witaj, Anonymous!". Jednak po dodaniu argumentu "Jan", wyświetli się "Witaj, Jan!".

## Deep Dive

W języku TypeScript można również użyć biblioteki "yargs" do czytania argumentów wiersza poleceń. Oferuje ona wiele przydatnych metod, które ułatwiają pracę z argumentami, takie jak tworzenie flag lub wymaganych argumentów. Oto przykładowe wykorzystanie biblioteki "yargs":

```TypeScript
import * as yargs from 'yargs';

const args = yargs.argv;
const firstName = args.firstname || 'Anonymous';
const lastName = args.lastname || 'Anonymous';

console.log(`Twoje imię to ${firstName} ${lastName}.`);
```

W powyższym kodzie, jeśli na przykład przekażesz argumenty "node index.ts --firstname Jan --lastname Kowalski", zostanie wyświetlony tekst "Twoje imię to Jan Kowalski.".

## Zobacz także

- [Dokumentacja modułu "process" w TypeScript](https://www.typescriptlang.org/docs/handbook/process.html)
- [Dokumentacja biblioteki "yargs"](https://yargs.js.org/docs/)