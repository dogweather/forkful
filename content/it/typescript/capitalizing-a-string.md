---
title:                "Capitalizzare una stringa"
html_title:           "TypeScript: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Probabilmente avrai mai incontrato una situazione in cui avresti voluto utilizzare una stringa con la prima lettera maiuscola, come ad esempio "Nome Utente" invece di "nome utente". Con TypeScript, è possibile farlo facilmente utilizzando alcuni metodi di manipolazione delle stringhe. In questo articolo vedremo come farlo e scopriremo anche alcune informazioni più approfondite su come funzionano le stringhe in TypeScript.

## Come Usare

Per prima cosa, è necessario assicurarsi di avere il compilatore TypeScript installato. Puoi farlo seguendo la guida ufficiale di TypeScript [qui](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html).

Una volta che il compilatore è installato, puoi iniziare a creare un nuovo progetto TypeScript. Apri il tuo editor di testo preferito e crea un nuovo file con l'estensione ".ts". Questo è il formato di file che TypeScript utilizza per i suoi progetti.

***Nota:*** *Se vuoi solo eseguire brevi comandi TypeScript, puoi anche utilizzare il compilatore online di TypeScript [qui](https://www.typescriptlang.org/play/).*

Iniziamo creando una variabile con una stringa all'interno. Puoi farlo digitando il seguente codice:

```TypeScript
let stringa = "prova";
```

Per ottenere una stringa con la prima lettera maiuscola, puoi utilizzare il metodo "charAt" per ottenere il carattere nella posizione desiderata e poi trasformarlo in maiuscolo utilizzando il metodo "toUpperCase". Ogni stringa in TypeScript ha un indice, in cui la prima lettera ha indice 0, la seconda 1 e così via.

Quindi, nel nostro esempio, possiamo utilizzare questo codice per ottenere la nostra stringa con la prima lettera maiuscola:

```TypeScript
let stringaPrimaLetteraMaiuscola = stringa.charAt(0).toUpperCase() + stringa.slice(1);
```

Questo prende il primo carattere della nostra stringa, il "p", lo trasforma in maiuscolo utilizzando il metodo "toUpperCase" e poi aggiunge il resto della stringa (ignorando il primo carattere) utilizzando il metodo "slice". Il risultato sarà una nuova stringa con la prima lettera maiuscola.

Output:
```TypeScript
console.log(stringaPrimaLetteraMaiuscola); // Prova
```

## Approfondimento

Oltre al metodo utilizzato sopra, TypeScript offre anche altre opzioni per manipolare le stringhe. Ad esempio, è possibile usare il metodo "replace" per sostituire parti di una stringa con un'altra. Puoi vedere tutti i metodi disponibili per le stringhe in TypeScript nella [documentazione ufficiale](https://www.typescriptlang.org/docs/handbook/strings.html).

Un'altra cosa interessante da notare è che in TypeScript è possibile utilizzare le stringhe template, che permettono di creare stringhe complesse, inclusi variabili e espressioni. Questo è possibile mediante l'utilizzo di virgolette inverse ("backticks") per delimitare la stringa e l'utilizzo di variabili all'interno della stringa delimitate da parentesi graffe.

Ecco un esempio di come utilizzare le stringhe template:

```TypeScript
let nome = "Mario";
let stringaTemplate = `Ciao ${nome}, benvenuto!`;
console.log(stringaTemplate); // Ciao Mario, benvenuto!
```

## Vedi Anche

- [Guida ufficiale di TypeScript](https://www.typescriptlang.org/docs/) 
- [Documentazione ufficiale su manipolazione delle stringhe in TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Compilatore online di TypeScript](https://www.typescriptlang.org/play/)