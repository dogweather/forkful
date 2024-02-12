---
title:                "Utilizzare un shell interattivo (REPL)"
date:                  2024-02-01T22:03:39.630592-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzare un shell interattivo (REPL)"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è e Perché?

Una shell interattiva, o Loop di Lettura-Valutazione-Stampa (REPL dall'inglese Read-Eval-Print Loop), è un ambiente di programmazione semplice e interattivo che accoglie singoli input dell'utente (espressioni), li valuta e restituisce il risultato all'utente. I programmatori utilizzano i REPL per prototipazione rapida, debug e per imparare la sintassi e il comportamento di un linguaggio di programmazione interattivamente.

## Come fare:

Google Apps Script, un linguaggio di scripting basato sul cloud per automatizzare compiti attraverso i prodotti Google, non dispone di uno strumento REPL integrato simile a quelli presenti in linguaggi come Python o il Node.js di JavaScript. Tuttavia, è possibile simulare un'esperienza simile utilizzando le funzionalità di registrazione e debug dell'editor di Apps Script o configurando un ambiente esterno. Qui, ci concentriamo sulla creazione di un REPL di fortuna all'interno dell'editor di Apps Script.

1. **Creare una funzione REPL di fortuna**:

```javascript
function myREPL() {
  var input = Logger.log('Inserisci la tua espressione: ');
  try {
    var result = eval(input);
    Logger.log('Risultato: ' + result);
  } catch(e) {
    Logger.log('Errore: ' + e.message);
  }
}
```

Poiché l'inserimento diretto dell'utente non è fattibile nello stesso modo di un REPL tradizionale nell'ambiente di Apps Script, è possibile modificare manualmente la variabile `input` ed eseguire `myREPL()` per testare le espressioni.

2. **Esecuzione di codice di esempio**:

Supponiamo che tu voglia valutare `2+2`. Dovresti modificare la funzione `myREPL` come segue:

```javascript
function myREPL() {
  var input = '2+2'; // Inserisci manualmente la tua espressione qui
  // Il resto rimane invariato...
}
```

Dopo aver eseguito `myREPL()`, controlla i Log (Visualizza > Log) per l'output, che dovrebbe leggere qualcosa come:

```
[20-xx-xxxx xx:xx:xx:xxx] Inserisci la tua espressione:
[20-xx-xxxx xx:xx:xx:xxx] Risultato: 4
```

3. **Debug con Logger**:

Per un debug più complesso, interspergi `Logger.log(variabile);` nel tuo codice per stampare gli stati delle variabili, aiutandoti a comprendere il flusso e gli stati intermedi dei tuoi script.

## Approfondimento

Il concetto di REPL è profondamente radicato nella storia dell'informatica, derivando dai sistemi a tempo condiviso degli anni '60 che permettevano sessioni interattive. Linguaggi come Lisp prosperavano in questo ambiente, poiché il REPL era fondamentale per il loro processo di sviluppo iterativo. Al contrario, Google Apps Script, emerso molto più tardi, è progettato principalmente per il web, concentrato sull'automazione di compiti all'interno della suite di Google più che sulla programmazione iterativa basata sulla console.

Google Apps Script non supporta tradizionalmente sessioni di codifica interattive e in tempo reale "out of the box" a causa della sua natura basata sul cloud e dell'enfasi sulla distribuzione di app web. Il suo modello di esecuzione ruota attorno a funzioni attivate da eventi web, trigger temporizzati o invocazione manuale all'interno dell'ambiente, piuttosto che cicli di feedback istantanei forniti da un REPL.

Sebbene il REPL di fortuna e il debugger all'interno dell'Editor di Apps Script offrano un certo livello di interattività, non replicano completamente il feedback immediato e l'efficienza dei REPL tradizionali presenti in molti linguaggi di programmazione. Gli sviluppatori che cercano un'esperienza REPL più autentica con le tecnologie di Google potrebbero esplorare ambienti JavaScript esterni o Node.js con le API di Google. Questi possono fornire una sessione di codifica più reattiva e interattiva, sebbene richiedano una configurazione maggiore e potenzialmente un'uscita dall'ambiente diretto di Apps Script.
