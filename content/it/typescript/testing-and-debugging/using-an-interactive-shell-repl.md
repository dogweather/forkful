---
title:                "Utilizzo di un interprete interattivo (REPL)"
aliases: - /it/typescript/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:18:29.873282-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un interprete interattivo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Un Read-Eval-Print-Loop (REPL) è un ambiente di programmazione che prende singoli input dall'utente, li esegue e restituisce il risultato all'utente. I programmatori utilizzano un REPL per sperimentare rapidamente con frammenti di codice, per il debug e per imparare le nuove funzionalità del linguaggio senza l'onere di creare un'applicazione completa.

## Come fare:
TypeScript non viene fornito con un proprio REPL. Utilizziamo `ts-node`, un ambiente di esecuzione TypeScript per Node.js che include un REPL.

Prima, installalo globalmente:
```bash
npm install -g ts-node
```

Avvia il REPL digitando `ts-node` nella tua riga di comando:
```bash
ts-node
```

Ecco un breve frammento da provare:
```TypeScript
> let message: string = 'Ciao, REPL!';
> console.log(message);
Ciao, REPL!
>
```
Per terminare la sessione, premi `Ctrl+D`.

## Approfondimento
Storicamente, i REPL erano diffusi in linguaggi come Lisp, consentendo la valutazione dinamica del codice. Il concetto si è poi diffuso, diventando un pilastro per la codifica interattiva in molti linguaggi.

Per TypeScript, `ts-node` non è la tua unica opzione. Le alternative includono l'uso del Playground TypeScript in un browser web o l'approfittare di altri REPL basati su Node.js che supportano TypeScript con plugin adatti.

In termini di implementazione, `ts-node` utilizza l'API del compilatore TypeScript per trascrivere il codice al volo prima che sia eseguito da Node.js. Questo ti dà un feedback immediato ed è particolarmente utile per provare le ultime funzionalità di TypeScript senza problemi di configurazione.

Una cosa da ricordare - mentre un REPL è ottimo per test rapidi, non sostituisce la scrittura di codice tradizionale, testabile e mantenibile. È uno strumento per l'apprendimento e l'esplorazione, non un sostituto per le pratiche di sviluppo adeguate.

## Vedi Anche
- [Sito Ufficiale di TypeScript](https://www.typescriptlang.org/)
- [ts-node su GitHub](https://github.com/TypeStrong/ts-node)
- [Documentazione REPL di Node.js](https://nodejs.org/api/repl.html)
- [Playground TypeScript](https://www.typescriptlang.org/play)
