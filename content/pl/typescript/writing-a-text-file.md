---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Zapisywanie do pliku tekstowego to sposób na trwałe umieszczenie danych, które można potem odczytać lub przetworzyć. Programiści robią to, by zapisywać logi, eksportować dane albo przechowywać konfiguracje.

## How to (Jak to zrobić):
Aby zapisać tekst do pliku w TypeScript, używamy wbudowanego modułu `fs`. Oto przykład:

```TypeScript
import * as fs from 'fs';

const data: string = 'To jest testowy tekst.';

fs.writeFile('plik.txt', data, (err) => {
    if (err) throw err;
    console.log('Plik został zapisany!');
});
```

Wykonanie powyższego kodu zapisze "To jest testowy tekst." do pliku `plik.txt`.

## Deep Dive (Dogłębna analiza):
Historia: TypeScript, stworzony przez Microsoft w 2012 r., rozwija standardy JavaScript, dodając typowanie statyczne. Zapisywanie plików tekstowych w JavaScript i TypeScript jest realizowane za pomocą API Node.js, nie różni się między tymi językami.

Alternatywy: Oprócz `fs.writeFile`, istnieje `fs.writeFileSync` dla operacji synchronicznych oraz biblioteki trzecie jak `fs-extra`, które dodają dodatkowe funkcje.

Szczegóły implementacji: Asynchroniczna funkcja `writeFile` używa callbacków do obsługi zakończenia operacji lub błędów, co jest typowe dla Node.js. Nowsze podejścia obejmują użycie `Promises` lub `async/await`.

## See Also (Zobacz także):
- Dokumentacja Node.js dla modułu `fs`: https://nodejs.org/api/fs.html
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
- O użyciu `Promises` i `async/await` w TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html
