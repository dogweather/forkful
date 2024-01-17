---
title:                "Convertire una stringa in minuscolo"
html_title:           "TypeScript: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cos'è e perché si fa?
Convertire una stringa in minuscolo è una pratica comune tra i programmatori per ottenere una versione di una stringa in cui tutte le lettere sono scritte in minuscolo. Questo può essere utile in molte situazioni, come nel confrontare due stringhe senza considerare le maiuscole e minuscole o nel rendere uniforme l'input degli utenti.

## Come fare:
```TypeScript
// Utilizzo del metodo toLowerCase() su una variabile stringa:
let nome = "Maria";
console.log(nome.toLowerCase()); // output: "maria"

// Utilizzo del ciclo for per convertire tutte le lettere di una stringa in minuscolo:
let stringa = "BENVENUTO";
let nuovaStringa = "";
for (let i = 0; i < stringa.length; i++) {
  nuovaStringa += stringa[i].toLowerCase();
}
console.log(nuovaStringa); // output: "benvenuto"
```

## Approfondimento:
Il metodo toLowerCase() è disponibile in molte lingue di programmazione, incluso TypeScript. Tuttavia, è importante notare che in alcune lingue ci sono alternative, come il metodo toLowerCaseLocale(). Inoltre, è possibile ricreare il funzionamento del metodo toLowerCase() utilizzando il codice Unicode delle lettere per convertire una stringa in minuscolo.

## Vedi anche:
- [Documentazione di TypeScript su toLowerCase()](https://www.typescriptlang.org/docs/handbook/strings.html#changing-case)
- [Articolo su alternative a toLowerCase()](https://gomakethings.com/the-case-insensitive-end-with-method-in-javascript/)