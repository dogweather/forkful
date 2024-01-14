---
title:    "TypeScript: Zapisywanie do standardowego błędu"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Standardowe wyjście jest jednym z podstawowych elementów w każdym języku programowania. Jednak, kiedy chcemy przekazać informacje o błędach, niezbędne jest skorzystanie ze standardowego wyjścia błędu lub "standard error". W tym artykule dowiesz się dlaczego jest to ważne i jak to zrobić w języku TypeScript.

## Jak to zrobić

Aby napisać informację do standardowego wyjścia błędu w TypeScript, musimy skorzystać z obiektu process.stderr. Wykorzystujemy do tego metodę write, która przyjmuje jedną lub więcej wartości jako argumenty. Poniżej znajduje się przykładowy kod oraz jego wynik w terminalu.

```TypeScript
import * as process from 'process';

process.stderr.write('To jest błąd');
```

```
To jest błąd
```

Jeśli chcemy przekazać bardziej szczegółową informację, możemy skorzystać z szablonów stringów i przekazać zmienne wewnątrz tekstu.

```TypeScript
import * as process from 'process';

const kod = 123;

process.stderr.write(`Kod błędu: ${kod}`);
```

```
Kod błędu: 123
```

## Głębsza analiza

Pisanie do standardowego wyjścia błędu jest szczególnie przydatne w przypadku obsługi błędów w naszym kodzie. Jest to również ważne w środowiskach produkcyjnych, gdzie musimy mieć możliwość szybkiego zlokalizowania i naprawienia błędów. Niektóre aplikacje wykorzystują również standardowe wyjście błędu do zapisywania logów, co może ułatwić diagnostykę problemów.

W języku TypeScript mamy również dostęp do wyjątków, które pozwolą nam na bardziej precyzyjne przekazywanie informacji o błędzie. Możemy skorzystać z klasy Error, aby utworzyć własny obiekt błędu i przekazać go do standardowego wyjścia błędu.

```TypeScript
import * as process from 'process';

class MojBlad extends Error {
  constructor(kod: number, wiadomosc: string) {
    super(wiadomosc);
    this.name = 'MojBlad';
    this.code = kod;
  }
}

const mojBlad = new MojBlad(123, 'To jest mój błąd');

process.stderr.write(mojBlad.message);
```

```
To jest mój błąd
```

## Zobacz również

* Dokumentacja w języku TypeScript na temat standardowego wyjścia błędu: https://nodejs.org/api/process.html#process_process_stderr
* Praktyczny przykład wykorzystania standardowego wyjścia błędu w Node.js: https://blog.risingstack.com/node-js-logging-tutorial/
* Artykuł na temat wyjątków w języku TypeScript: https://blog.logrocket.com/error-handling-in-typescript/