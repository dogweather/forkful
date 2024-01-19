---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Leggere gli argomenti della riga di comando, in Javascript, significa accedere ai dati passati a un'app durante l'avvio. I programmatori lo fanno per personalizzare l'esecuzione e rendere il codice più versatile.

## Come Si Fa:

Utilizziamo `process.argv` in Node.js per leggere gli argomenti della riga di comando. Ogni elemento del vettore `process.argv` rappresenta un argomento.

```Javascript
// myscript.js
console.log(process.argv)
```
Se eseguiamo `node myscript.js uno due tre`, vedremo:
```Javascript
[
  '/usr/local/bin/node',
  '/Users/tuo-username/myscript.js',
  'uno',
  'due',
  'tre'
]
```
I primi due elementi sono il percorso di Node e il percorso dello script. I dati che ci interessano iniziano dal terzo elemento, `process.argv[2]`.

## Approfondimenti:

Historicamente, leggere gli argomenti della riga di comando è stato standard fin dalle prime interfacce a riga di comando.

Anche se `process.argv` è la metodologia nativa, esistono alternative più raffinate, come la libreria `yargs`, che fornisce un'analisi più robusta degli argomenti.

Ricorda, però, che c'è una differenza nella gestione degli argomenti quando si esegue il codice direttamente nel browser. Infatti, `process.argv` è specifico per Node.js e non funziona nel browser JavaScript per motivi di sicurezza.

## Vedere Anche:

1. [Node.js process.argv](https://nodejs.org/api/process.html#process_process_argv)
2. [yargs su npm](https://www.npmjs.com/package/yargs)
3. [Javascript nei browser su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Guide/Introduction)