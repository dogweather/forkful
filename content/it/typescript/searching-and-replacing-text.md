---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La ricerca e la sostituzione del testo è un'operazione fondamentale per manipolare le stringhe nei programmi. I programmatori la utilizzano per trovare pattern di testo specifici e quindi modificarli o sostituirli con qualcos'altro.

## Come fare:

Possiamo usare il metodo `replace()` di JavaScript per cercare e sostituire testo all'interno delle stringhe in TypeScript. Ecco un semplice esempio:

```TypeScript
let frase: string = "Ciao, mondo!";
let nuovaFrase: string = frase.replace("mondo", "Italia");
console.log(nuovaFrase); // Stampa: "Ciao, Italia!"
```

## Approfondimento

La ricerca e la sostituzione di testo è un concetto vecchio quanto la programmazione stessa. Nel contesto di JavaScript (e TypeScript), la funzione `replace()` è una delle prime funzioni di manipolazione delle stringhe introdotte. 

Esistono altre alternative per la ricerca e la sostituzione del testo, come l'uso delle espressioni regolari che potrebbero essere più potenti in certi scenari. Ad esempio:

```TypeScript
let frase: string = "Ciao, mondo! mondo!";
let pattern: RegExp = /mondo/g;
let nuovaFrase: string = frase.replace(pattern, "Italia");
console.log(nuovaFrase); // Stampa: "Ciao, Italia! Italia!"
```

Nel contesto di implementazione, il metodo `replace()` funziona sostituendo solo la prima istanza del valore di ricerca nella stringa. Se vuoi sostituire tutte le istanze, ha bisogno di utilizzare un'espressione regolare con un flag globale `g`, come mostrato nell'esempio precedente.

## Vedi anche

- Documentazione Microsoft su TypeScript: https://www.typescriptlang.org/docs
- Metodi di stringa in JavaScript: https://developer.mozilla.org/it/docs/Web/JavaScript/Guida/Tipi_di_dato_e_operazioni#Stringhe
- Espressioni regolari in JavaScript: https://developer.mozilla.org/it/docs/Web/JavaScript/Guida/Espressioni_Regolari