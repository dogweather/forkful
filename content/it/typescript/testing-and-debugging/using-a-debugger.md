---
date: 2024-01-26 04:11:08.537166-07:00
description: "Un debugger \xE8 uno strumento che consente di esaminare e modificare\
  \ il funzionamento interno del proprio codice mentre viene eseguito. I programmatori\
  \ lo\u2026"
lastmod: '2024-03-11T00:14:16.742562-06:00'
model: gpt-4-0125-preview
summary: "Un debugger \xE8 uno strumento che consente di esaminare e modificare il\
  \ funzionamento interno del proprio codice mentre viene eseguito. I programmatori\
  \ lo\u2026"
title: Utilizzo di un debugger
---

{{< edit_this_page >}}

## Cos'è & Perché?
Un debugger è uno strumento che consente di esaminare e modificare il funzionamento interno del proprio codice mentre viene eseguito. I programmatori lo usano per eliminare bug passo dopo passo attraverso il loro codice, ispezionando le variabili e comprendendo il flusso del loro programma.

## Come fare:

Per iniziare con un debugger in TypeScript, tutto ciò che serve è un IDE supportato (come Visual Studio Code) e una configurazione `launch.json`. Ecco un esempio rapido per un'applicazione Node.js:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Ciao, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

Per eseguire il debug, crea un file `launch.json` nella cartella `.vscode`:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Avvia Programma",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Quindi, imposta un punto di interruzione nella tua funzione `greet` cliccando sul lato sinistro del numero di linea nel tuo IDE. Premi F5 per iniziare il debug e osserva la tua app fermarsi al punto di interruzione. Ora puoi passare il mouse sopra le variabili, osservare espressioni e passare attraverso il tuo codice con facilità.

## Approfondimento

Ai vecchi tempi, prima che gli ambienti di sviluppo integrati (IDE) diventassero sofisticati, il debugging era spesso eseguito con istruzioni di stampa (il cosiddetto debugging con `console.log`). Funzionava, in un certo senso, ma era come cercare un ago in un pagliaio bendati.

I debugger moderni sono come un coltellino svizzero per la risoluzione dei problemi. Con l'evoluzione di TypeScript e Node.js, sono disponibili vari debugger, dall'ispettore Node.js integrato agli strumenti di sviluppo del browser per il debugging lato client.

L'ispettore di Node.js funziona collegandosi all'applicazione in esecuzione; comunica tramite il Protocollo DevTools di Chrome, trasformando il browser Chrome in una potente console di debugging. Questa integrazione consente una sessione di debugging visivamente interattiva e dettagliata, rispetto alle pratiche tradizionali di debugging da riga di comando.

## Vedi Anche

Per un po' di lettura aggiuntiva e alcuni consigli da professionisti, controlla:

- [Debugging di TypeScript in Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Guida al Debugging di Node.js](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Documentazione su Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
