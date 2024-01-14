---
title:    "TypeScript: Lettura degli argomenti della riga di comando"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché leggere gli argomenti della riga di comando

Leggere gli argomenti della riga di comando è un'abilità essenziale per qualsiasi programmatore TypeScript. Questo consente di interagire con il programma e passare informazioni specifiche durante l'esecuzione. Continua a leggere per imparare come farlo!

## Come leggere gli argomenti della riga di comando

Per leggere gli argomenti della riga di comando in TypeScript, è necessario utilizzare il modulo process. Questo modulo contiene la proprietà argv, che è un array di stringhe contenente gli argomenti della riga di comando. Ecco un esempio di come utilizzarlo:

```typescript
const args = process.argv;
console.log(args);
```

Se eseguiamo questo codice da linea di comando, passando ad esempio `node app.ts arg1 arg2`, otterremo il seguente output:

```javascript
['/usr/bin/node', '/path/to/app.ts', 'arg1', 'arg2']
```

Come puoi vedere, viene stampato l'array contenente sia il percorso del nodo che il percorso del tuo file, seguiti dagli argomenti passati.

## Deep Dive

Oltre a leggere gli argomenti della riga di comando, è possibile anche manipolarli e accedere a valori specifici. Ad esempio, se vogliamo accedere al primo argomento passato, possiamo farlo utilizzando `process.argv[2]`. Inoltre, è possibile utilizzare cicli e condizioni per gestire gli argomenti in modo dinamico all'interno del codice TypeScript. 

## Vedi anche

- [Documentazione TypeScript: process.argv](https://www.typescriptlang.org/docs/handbook/command-line-arguments.html#accessing-command-line-arguments-in-a-typescript-program)
- [Tutorial di Node.js su come leggere gli argomenti della riga di comando](https://www.nodebeginner.org/es/nodejs-esencial/tutorial-de-node.js/como-leer-argumentos-de-la-linea-de-comandos.html)
- [Guida agli argomenti della riga di comando in TypeScript](https://attacomsian.com/blog/command-line-arguments-typescript)