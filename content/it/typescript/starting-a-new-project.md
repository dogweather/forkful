---
title:                "Avviare un nuovo progetto"
html_title:           "TypeScript: Avviare un nuovo progetto"
simple_title:         "Avviare un nuovo progetto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui qualcuno potrebbe decidere di iniziare un nuovo progetto utilizzando TypeScript. Innanzitutto, TypeScript è un linguaggio di programmazione moderno e potente che sta diventando sempre più popolare tra gli sviluppatori. Inoltre, l'utilizzo di TypeScript può aumentare la produttività e la qualità del codice grazie alla sua tipizzazione statica e alle numerose funzionalità avanzate.

## Come iniziare

Per iniziare un nuovo progetto TypeScript, è necessario assicurarsi di avere installato l'ultima versione di Node.js, che è disponibile gratuitamente sul sito ufficiale. Successivamente, è possibile utilizzare il comando `npm` per installare TypeScript globalmente sul proprio sistema:

```TypeScript
npm install -g typescript
```

Una volta completata l'installazione, è possibile creare una nuova cartella per il progetto e all'interno di essa inizializzare un nuovo progetto TypeScript utilizzando il comando `tsc --init`. Questo creerà un file `tsconfig.json` che conterrà le impostazioni del progetto. Ora è possibile scrivere il codice TypeScript all'interno di file con estensione `.ts` e utilizzare il comando `tsc` per compilare il codice in JavaScript:

```TypeScript
tsc nomefile.ts
```

Il codice compilato verrà salvato in un nuovo file con estensione `.js`. Ora è possibile eseguire il file JavaScript utilizzando Node.js, come si farebbe con qualsiasi altro progetto JavaScript. Ad esempio:

```TypeScript
console.log("Ciao, mondo!");
```

## Approfondimento

Se si desidera approfondire ulteriormente le funzionalità di TypeScript, è possibile consultare la documentazione ufficiale su TypeScript o guardare alcuni tutorial online. Inoltre, esistono numerosi framework e librerie che supportano TypeScript, come Angular, React e Vue. Utilizzare uno di questi framework può semplificare notevolmente lo sviluppo di un progetto TypeScript.

## Vedi anche

- Documentazione ufficiale TypeScript: https://www.typescriptlang.org/docs/
- Tutorial TypeScript: https://www.tutorialspoint.com/typescript/index.htm
- Framework Angular: https://angular.io/
- Libreria React: https://reactjs.org/
- Framework Vue: https://vuejs.org/