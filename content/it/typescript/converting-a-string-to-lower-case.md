---
title:                "TypeScript: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una stringa in lettere minuscole è un'operazione comune nella programmazione. Ciò può essere utile per confrontare stringhe in modo non sensibile alle maiuscole/minuscole o per formattare i dati in un modo specifico. In questo articolo, vedremo come eseguire questa operazione in TypeScript.

## Come fare

Per convertire una stringa in lettere minuscole in TypeScript, possiamo utilizzare il metodo `toLowerCase()` della classe `String`. Questo metodo restituisce una nuova stringa con tutti i caratteri in lettere minuscole.

Ecco un esempio di codice che utilizza il metodo `toLowerCase()`:

```TypeScript
let stringa: string = "CIAO AMICI";
let stringaInMinuscolo: string = stringa.toLowerCase();
console.log(stringaInMinuscolo); // output: ciao amici
```

## Approfondimento

In alcuni casi, potresti voler mantenere solo la prima lettera di una stringa in maiuscolo e convertire il resto in minuscolo. In TypeScript, possiamo farlo utilizzando il metodo `toUpperCase()` della classe `String` e poi abbassando le lettere restanti con `toLowerCase()`.

Ecco un esempio:

```TypeScript
let nome: string = "mARiA";
let nomeFormattato: string = nome[0].toUpperCase() + nome.slice(1).toLowerCase();
console.log(nomeFormattato); // output: Maria
```

Questa tecnica può essere utile quando si deve formattare un input di un utente, come ad esempio un nome o un cognome.

## Vedi anche

- Documentazione su `toLowerCase()`: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Documentazione su `toUpperCase()`: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase