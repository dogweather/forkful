---
title:                "Cancellando caratteri corrispondenti a un modello"
html_title:           "TypeScript: Cancellando caratteri corrispondenti a un modello"
simple_title:         "Cancellando caratteri corrispondenti a un modello"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & perché?
Eliminare i caratteri che corrispondono a un certo modello è un'operazione molto utile per i programmatori. Ciò significa rimuovere dall'input una serie di caratteri specifici che soddisfano determinati criteri. Ciò può essere fatto per semplificare o elaborare l'input al fine di ottenere un output desiderato.

## Come fare:
Ecco un esempio di codice TypeScript per eliminare i caratteri che corrispondono a un modello:

```TypeScript
let input = "Ho voglia di una pizza pepperoni!";
let output = input.replace(/e/g, "");
console.log(output); // Stampa "Ho voglia di una pizza ppproni!"
```

In questo esempio, il metodo "replace" è usato per sostituire tutti i caratteri "e" nell'input con una stringa vuota, eliminandoli così dall'output.

## Approfondimento:
Eliminare i caratteri che corrispondono a un modello è diventato più facile grazie all'avvento delle espressioni regolari (regex) nei linguaggi di programmazione. Le espressioni regolari consentono di specificare pattern complessi che possono corrispondere a diversi tipi di caratteri. Inoltre, esistono alternative per eliminare i caratteri che corrispondono a un modello, come l'utilizzo di metodi di stringa come "split" e "substring" in TypeScript.

## Vedi anche:
- Documentazione ufficiale di TypeScript sul metodo "replace": https://www.typescriptlang.org/docs/handbook/declarations.html#replace
- Tutorial su come utilizzare espressioni regolari in TypeScript: https://www.tutorialspoint.com/typescript/typescript_regular_expressions.htm
- Altri metodi per manipolare le stringhe in TypeScript: https://www.geeksforgeeks.org/how-to-manipulate-strings-in-typescript/