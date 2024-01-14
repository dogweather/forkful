---
title:    "TypeScript: Capitalizzare una stringa"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa è un'operazione comune quando si lavora con le stringhe in TypeScript. Ciò rende il testo più leggibile e coerente. Inoltre, molte funzionalità e librerie richiedono stringhe capitalizzate per funzionare correttamente. Quindi, è importante sapere come farlo correttamente.

## Come

Per capitalizzare una stringa in TypeScript, possiamo utilizzare il metodo `toUpperCase()` insieme alla proprietà `charAt()` per ottenere il primo carattere della stringa e convertirlo in maiuscolo. Quindi possiamo concatenare il resto della stringa utilizzando il metodo `slice()`.

```TypeScript
let stringa = "esempio"
stringa = stringa.charAt(0).toUpperCase() + stringa.slice(1)
console.log(stringa)
// Output: Esempio
```

Possiamo anche utilizzare il metodo `replace()` insieme a una espressione regolare per sostituire il primo carattere con la versione maiuscola.

```TypeScript
let stringa = "un altro esempio"
stringa = stringa.replace(/^./, stringa.charAt(0).toUpperCase())
console.log(stringa)
// Output: Un altro esempio
```

## Deep Dive

Ci sono anche altre opzioni per capitalizzare una stringa in TypeScript. Ad esempio, possiamo utilizzare la libreria `lodash` e il suo metodo `capitalize()` che gestisce anche le stringhe con più parole.

```TypeScript
import { capitalize } from 'lodash'
let stringa = "una stringa con più parole"
stringa = capitalize(stringa)
console.log(stringa)
// Output: Una stringa con più parole
```

Possiamo anche creare una funzione di utilità che sfrutta la ricorsione per capitalizzare ogni parola della stringa.

```TypeScript
// Funzione di utilità per capitalizzare una stringa con più parole
function capitalizeWords(str: string): string {
    return str.replace(/\b\w/g, (l) => l.toUpperCase())
}

let stringa = "un'altra stringa con più parole"
stringa = capitalizeWords(stringa)
console.log(stringa)
// Output: Un'Altra Stringa Con Più Parole
```

## Vedi anche

- [String.prototype.toUpperCase() su developer.mozilla.org](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String.prototype.charAt() su developer.mozilla.org](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.slice() su developer.mozilla.org](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [String.prototype.replace() su developer.mozilla.org](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Metodi per capitalizzare una stringa su stackoverflow.com](https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript)