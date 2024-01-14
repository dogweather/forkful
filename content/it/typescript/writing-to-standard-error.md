---
title:    "TypeScript: Scrivere in standard error"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere a stderr (standard error) è un modo per gestire gli errori in un programma TypeScript. Quando il codice ha incontrato un errore durante l'esecuzione, scrivere a stderr consente di informare l'utente e raccogliere informazioni sull'errore per risolverlo.

## Come fare

Scrivere a stderr in TypeScript è semplice. Basta utilizzare la funzione `process.stderr.write()` e passare il messaggio da scrivere come argomento. Ecco un esempio di codice:

```TypeScript
const num1 = 10;
const num2 = 0;

if (num2 === 0) {
  // scrive il messaggio di errore a stderr
  process.stderr.write("Impossibile dividere per 0");
} else {
  const result = num1 / num2;
  console.log(result); // output: Infinity
}
```

Quando il codice viene eseguito, verrà visualizzato il messaggio di errore a stderr e il risultato della divisione verrà stampato sulla console. Ecco un'esempio di output:

```
Impossibile dividere per 0
Infinity
```

## Approfondimento

Ci sono altre opzioni per gestire gli errori e scrivere a stderr in TypeScript. Ad esempio, è possibile utilizzare la classe `console` di Node.js e il metodo `error()` per scrivere a stderr. Inoltre, è possibile personalizzare il messaggio di errore passando un secondo argomento alla funzione `write()`, come mostrato di seguito:

```TypeScript
const num1 = 10;
const num2 = 0;

if (num2 === 0) {
  // passa un codice di errore come secondo argomento
  process.stderr.write("Impossibile dividere per 0", "ERROR");
} else {
  const result = num1 / num2;
  console.log(result); // output: Infinity
}
```

L'output di questo codice sarà il seguente:

```
ERROR: Impossibile dividere per 0
Infinity
```

Inoltre, è possibile utilizzare la funzione `process.exit()` per terminare l'esecuzione del programma in caso di errore e fornire un codice di uscita specifico per indicare il tipo di errore.

## Vedi anche

- [La documentazione ufficiale di Node.js su stdout e stderr](https://nodejs.org/api/process.html#process_a_note_on_process_i_o)
- [Un articolo su come gestire gli errori in TypeScript](https://blog.logrocket.com/error-handling-typescript-node-js/)
- [Un tutorial su come utilizzare la classe console in Node.js](https://www.digitalocean.com/community/tutorials/how-to-use-the-node-js-console)