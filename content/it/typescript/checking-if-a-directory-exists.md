---
title:                "Verifica se una directory esiste"
html_title:           "Lua: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Verificare se una directory esiste significa determinare se un determinato percorso nel sistema di file del tuo disco rigido contiene una cartella. È una pratica comune tra i programmatori per evitare errori legati all'accesso a directory inesistenti che possono causare problemi nel programma.

## Come fare:

In TypeScript, possiamo utilizzare il modulo `fs` del Node.js per verificare se una directory esiste. Ecco come:

```TypeScript
import { existsSync } from 'fs';

console.log(existsSync('/path/to/dir'));  // ritorna true se la directory esiste, altrimenti false
```
Ecco un esempio di output:

```TypeScript
true
```
## Approfondiamo

Verificare se una directory esiste è un'operazione fondamentale nell'interazione con il sistema di file. Negli anni, vari linguaggi di programmazione hanno fornito diversi metodi per eseguire questa verifica.

In TypeScript, l'opzione principale viene fornita dal modulo `fs` di Node.js. Tuttavia, vale la pena notare che l'utilizzo di 'existsSync' può portare a condizioni di gara, dove lo stato del file potrebbe cambiare tra il controllo e l'accesso successivo al file. In questi casi, potrebbe essere meglio semplicemente aprire il file e gestire un'eventuale eccezione se il file non esiste.

Un'altra alternativa è l'uso della funzione 'access' del modulo `fs`, che verifica i permessi di accesso per il percorso specificato, ma è più lento.

```TypeScript
import { access } from 'fs';

access('/path/to/dir', error => {
    if (error) {
        console.log("La directory non esiste");
    } else {
        console.log("La directory esiste");
    }
});
```

## Vedi anche

1. Documentazione Node.js `fs`: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
3. Stack Overflow - 'fs.existsSync vs fs.access': [https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js](https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js)