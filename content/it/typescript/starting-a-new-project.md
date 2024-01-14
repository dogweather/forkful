---
title:                "TypeScript: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Perché iniziare un nuovo progetto TypeScript

Iniziare un nuovo progetto in TypeScript può essere una scelta vantaggiosa per molti motivi. TypeScript è un linguaggio di programmazione che combina la tipizzazione statica di JavaScript con funzionalità di programmazione orientata agli oggetti, rendendolo più robusto ed efficiente.

Inoltre, TypeScript è compatibile con la maggior parte dei moderni framework di sviluppo web, come Angular e React, offrendo una maggiore flessibilità nella gestione e manutenzione del codice.

# Come iniziare un nuovo progetto TypeScript

Per iniziare un nuovo progetto in TypeScript, è necessario avere installato prima il compilatore TypeScript e un editor di testo o IDE. Una volta fatto ciò, è possibile seguire i seguenti passaggi:

1. Creare una nuova cartella per il progetto
2. Inizializzare il progetto con il comando `tsc --init`
3. Creare un file `index.ts`
4. Inserire il seguente codice:

```TypeScript
class Persona {
    nome: string;
    cognome: string;
    
    constructor(nome: string, cognome: string) {
        this.nome = nome;
        this.cognome = cognome;
    }

    presentazione(): void {
        console.log(`Ciao, mi chiamo ${this.nome} ${this.cognome}`);
    }
}

let persona = new Persona("Mario", "Rossi");
persona.presentazione();
```

5. Eseguire il comando `tsc index.ts` per compilare il file TypeScript in JavaScript
6. Eseguire il comando `node index.js` per eseguire il file JavaScript e visualizzare l'output:

```
Ciao, mi chiamo Mario Rossi
```

# Approfondimento

Per approfondire l'utilizzo di TypeScript in un nuovo progetto, è importante comprendere le nozioni di base del linguaggio come tipi di dati, classi, interfaccie e moduli. Inoltre, è possibile integrare TypeScript con altri strumenti come Babel per aumentare la compatibilità con i browser più vecchi.

Altre risorse utili da consultare sono:

- [Sito ufficiale di TypeScript](https://www.typescriptlang.org/)
- [Documentazione di TypeScript](https://www.typescriptlang.org/docs/)
- [Tutorial su TypeScript](https://www.tutorialspoint.com/typescript/index.htm)

# Vedi anche

- [Tutorial su Angular con TypeScript](https://www.tutorialspoint.com/angular6/typescript.htm)
- [Utilizzo di TypeScript con React](https://blog.logrocket.com/using-typescript-with-react/)
- [Guida introduttiva a Babel e TypeScript](https://medium.com/@iamcherta/getting-started-with-babel-and-typescript-714abfc8b3e)