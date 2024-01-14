---
title:                "TypeScript: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, nel processo di sviluppo di un'applicazione TypeScript, potremmo avere la necessità di estrarre una parte di una stringa e utilizzarla per diverse operazioni. Ciò potrebbe essere necessario per manipolare i dati in un formato specifico o per validare l'input dell'utente. In questo articolo scopriremo il motivo per cui è importante imparare a estrarre sottostringhe e come farlo correttamente in TypeScript.

## Come fare

Per estrarre una sottostringa da una stringa in TypeScript, possiamo utilizzare il metodo `substring()` o l'operatore `slice()`.

Esempio utilizzando `substring()`:

```TypeScript
let stringa = "Questo è un esempio di stringa.";

console.log(stringa.substring(6, 14)); //stampa "è un esempio"
```

Esempio utilizzando `slice()`:

```TypeScript
let stringa = "Questo è un esempio di stringa.";

console.log(stringa.slice(6, 14)); //stampa "è un esempio"
```

Entrambi i metodi richiedono come argomenti l'indice di partenza e l'indice di fine della sottostringa desiderata. È importante notare che il primo indice è incluso nella sottostringa, mentre il secondo no.

Inoltre, possiamo utilizzare anche l'indice negativo per indicare la posizione dall'ultima lettera della stringa. Ad esempio, `-1` rappresenta l'ultima lettera della stringa, `-2` la penultima e così via.

```TypeScript
let stringa = "Questo è un esempio di stringa.";

console.log(stringa.substring(-13, -5)); //stampa "è un esempio"
```

```TypeScript
let stringa = "Questo è un esempio di stringa.";

console.log(stringa.slice(-13, -5)); //stampa "è un esempio"
```

Questi metodi possono essere utili anche per sostituire una parte di una stringa con un'altra sottostringa, semplicemente utilizzando il metodo `replace()`.

```TypeScript
let stringa = "Questo è un esempio di stringa.";

console.log(stringa.replace("è un esempio", "è una stringa diversa")); 
//stampa "Questo è una stringa diversa di stringa."
```

## Approfondimento

L'estrazione di sottostringhe può essere ulteriormente personalizzata utilizzando la regola di espressione regolare `substr()`. Questa regola accetta anche un terzo parametro, rappresentante la lunghezza della sottostringa desiderata.

```TypeScript
let stringa = "Questo è un esempio di stringa.";

console.log(stringa.substr(6, 8)); //stampa "è un ese"
```

Questa regola può essere utile se conosciamo la posizione esatta dell'ultima lettera della sottostringa invece che la posizione di fine.

## Vedi anche

- [Documentazione ufficiale di TypeScript su substring() e slice()](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-subtypes)
- [Guida completa su espressioni regolari in TypeScript](https://www.sitepoint.com/expressions-regular-typescript/)
- [Utilizzo di substring, slice e substr per manipolare le stringhe in JavaScript](https://www.javascriptinplainenglish.com/extract-substring-javascript/)