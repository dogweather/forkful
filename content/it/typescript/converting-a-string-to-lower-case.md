---
title:                "TypeScript: Convertire una stringa in minuscolo"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con dati di input, si può ottenere una stringa in maiuscolo o nel formato sbagliato. Questo può portare a errori di confronto e ricerca all'interno del nostro codice. Per questo motivo, è importante conoscere come convertire una stringa in minuscolo in TypeScript.

## Come eseguire la conversione

Per convertire una stringa in minuscolo in TypeScript, si utilizza il metodo `toLowerCase()`. Questo metodo accetta una stringa come argomento e restituisce la stessa stringa ma in formato minuscolo. Vediamo un esempio:

```TypeScript
let inputString = "CODICE";
let outputString = inputString.toLowerCase();
console.log(outputString); // Output: codice
```

Come si può vedere dall'esempio, il metodo `toLowerCase()` ha convertito la stringa "CODICE" in "codice". È importante notare che questo metodo non modifica la stringa originale, ma ne restituisce una nuova. 

## Approfondimento

Esistono alcune eccezioni da considerare quando si esegue la conversione di una stringa in minuscolo. Ad esempio, la lettera "i" con puntini (o trema) in alcune lingue può risultare in una lettera diversa da "i" quando è maiuscola. Inoltre, ci possono essere delle differenze tra le codifiche dei caratteri che possono influenzare il risultato finale. 

In TypeScript, è anche possibile utilizzare il metodo `toLocaleLowerCase()` che tiene conto delle impostazioni regionali della lingua utilizzata nel sistema in cui viene eseguito il codice. Questo rende il processo di conversione più preciso e adatto anche per applicazioni multilingue.

## Vedi anche

- [Documentazione su toLowerCase() in TypeScript](https://www.typescriptlang.org/docs/handbook/variables.html#type-assertions)
- [Esempi di conversione di stringhe in TypeScript](https://dev.to/beninada/how-to-convert-string-to-lowercase-in-typescript-o08)
- [Documentazione su toLocaleLowerCase() in TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#string-operations)