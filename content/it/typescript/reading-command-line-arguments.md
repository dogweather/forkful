---
title:                "Leggere gli argomenti da riga di comando"
html_title:           "TypeScript: Leggere gli argomenti da riga di comando"
simple_title:         "Leggere gli argomenti da riga di comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore TypeScript alle prime armi che vuole imparare a utilizzare gli argomenti della riga di comando, sei nel posto giusto. Leggere gli argomenti della riga di comando può aiutarti a rendere i tuoi script più flessibili e interattivi, dando all'utente la possibilità di inserire input personalizzati.

## Come fare

Per leggere gli argomenti della riga di comando in TypeScript, è possibile utilizzare il modulo `process` che è disponibile di default nella maggior parte degli ambienti di esecuzione. Ecco un esempio di codice che mostra come leggere gli argomenti dalla riga di comando e stamparli a schermo:

```TypeScript
const arguments = process.argv.slice(2); // Rimuove i primi due argomenti (path di Node e path del file)
console.log(`I tuoi argomenti sono: ${arguments}`);
```

Output:

```
I tuoi argomenti sono: [arg1, arg2, arg3]
```

Se vuoi ottenere gli argomenti come singoli elementi invece di un array, puoi usare il metodo `join()` dopo aver utilizzato `slice()`.

## Approfondimento

Oltre ad essere in grado di leggere gli argomenti, è importante anche essere in grado di gestirli correttamente. TypeScript offre diverse opzioni per la gestione dei tipi delle variabili e quindi anche degli argomenti. Puoi specificare il tipo di una variabile utilizzando la sintassi `let variableName : type = value;` e aggiungendo il tipo dell'argomento tra parentesi tonde all'interno delle parentesi quadre durante la dichiarazione degli argomenti.

Se gli argomenti devono essere opzionali, puoi utilizzare il simbolo `?` dopo il nome dell'argomento per dichiararlo come opzionale.

```TypeScript
let name : string = "World";
let iterations : number = 1;

function greet(name : string, iterations? : number) {
    for(let i = 0; i < iterations; i++) {
        console.log(`Hello ${name}!`);
    }   
}

greet(name); // Output: Hello World!
```

## Vedi anche

Per ulteriori informazioni su come leggere gli argomenti della riga di comando in TypeScript, puoi consultare la documentazione ufficiale del modulo `process` qui: https://nodejs.org/api/process.html#process_process_argv

Puoi anche saperne di più su TypeScript in generale su https://www.typescriptlang.org/.