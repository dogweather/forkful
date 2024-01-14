---
title:    "TypeScript: Convertire una stringa in minuscolo"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in caratteri minuscoli è utile quando si vuole uniformare la formattazione o confrontare due stringhe in modo case-insensitive.

## Come fare

```TypeScript
let stringaOriginale = "HELLO WORLD";
let stringaMinuscola = stringaOriginale.toLowerCase();

console.log(stringaMinuscola); // output: hello world
```

In questo esempio, abbiamo dichiarato una variabile ```stringaOriginale``` contenente il valore "HELLO WORLD" e abbiamo utilizzato il metodo ```toLowerCase()```per convertire tutti i caratteri in minuscolo.

## Deep Dive

Quando si lavora con i dati, può essere utile avere una formattazione uniforme tra le stringhe in modo da semplificare le operazioni di confronto e ricerca. Inoltre, utilizzare il metodo ```toLowerCase()``` garantisce un confronto case-insensitive, rendendo la logica del nostro programma più robusta.

In TypeScript, il metodo ```toLowerCase()``` utilizza l'Unicode per la conversione dei caratteri in minuscolo, garantendo una formattazione coerente anche con caratteri speciali e accenti.

## Vedi anche

- [Metodo toLowerCase() di TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#lowercase-and-uppercase-strings)
- [Risorse su TypeScript per principianti](https://blog.logrocket.com/typescript-tutorial-for-beginners/)
- [Sito ufficiale di TypeScript](https://www.typescriptlang.org/)