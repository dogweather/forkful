---
title:    "Javascript: Lettura degli argomenti della riga di comando"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore JavaScript, probabilmente hai familiarità con l'utilizzo di JavaScript all'interno di un browser per creare interazioni dinamiche sulle pagine web. Ma ci sono molte altre applicazioni per questo linguaggio di programmazione, tra cui lo sviluppo di applicazioni server-side e script di utilità da eseguire a livello di sistema. Uno strumento fondamentale per lavorare su questi tipi di progetti è il comando di linea. In questo articolo, esploreremo come leggere gli argomenti della riga di comando utilizzando JavaScript.

## Come fare

Per leggere gli argomenti della riga di comando in JavaScript, possiamo utilizzare l'oggetto `process.argv`. Questo oggetto contiene un array di stringhe, dove la prima posizione è il percorso del file JavaScript che si sta eseguendo e le posizioni successive sono gli argomenti passati durante l'esecuzione del file.

```Javascript
const args = process.argv;
console.log(args[2]); // Stampa il primo argomento passato
```

Supponiamo di avere un file javascript chiamato `esempio.js` e di eseguirlo utilizzando il comando `node esempio.js arg1 arg2`. In questo caso, il codice sopra stamperebbe `arg1` nella console.

## Approfondimento

Oltre a leggere gli argomenti della riga di comando, possiamo anche analizzarli e utilizzarli per eseguire operazioni più complesse. Ad esempio, possiamo utilizzare i flag per attivare determinate funzioni o leggere input da file specifici. Vediamo un esempio di come possiamo usare questa funzionalità per creare un programma in grado di effettuare una semplice operazione matematica.

```Javascript
const args = process.argv;
const num1 = parseInt(args[2]);
const num2 = parseInt(args[3]);
const operator = args[4];

let result;

switch(operator){
  case '+':
    result = num1 + num2;
    break;
  case '-':
    result = num1 - num2;
    break;
  case '*':
    result = num1 * num2;
    break;
  case '/':
    result = num1 / num2;
    break;
  default:
    console.log('Operatore non valido');
}

console.log(result);
```

Nelle righe sopra, leggiamo tre argomenti dalla riga di comando: due numeri e un operatore. Successivamente, utilizziamo la dichiarazione switch per selezionare l'operazione da eseguire in base all'operatore passato come argomento. Infine, stampiamo il risultato nella console.

## Vedi anche

Se sei interessato a saperne di più sugli argomenti della riga di comando in JavaScript, questi articoli potrebbero essere utili:

- [Leggere gli argomenti della riga di comando con JavaScript](https://www.digitalocean.com/community/tutorials/reading-command-line-arguments-with-javascript)
- [Process.argv in Node.js](https://nodejs.org/api/process.html#process_process_argv)
- [Eseguire un file JavaScript dalla riga di comando](https://www.freecodecamp.org/news/how-to-run-a-javascript-file-from-the-command-line-in-linux/)