---
title:                "Registrazione delle Attività (Logging)"
aliases: - /it/haskell/logging.md
date:                  2024-01-26T01:06:56.981155-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione delle Attività (Logging)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/logging.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il logging nella programmazione è sostanzialmente come lasciare una traccia di briciole sotto forma di eventi o messaggi registrati, che possono essere utilizzati per monitorare cosa sta facendo la tua applicazione in ogni momento. I programmatori lo fanno per individuare e risolvere problemi, monitorare le prestazioni del sistema, e verificare il comportamento per ragioni di sicurezza e conformità.

## Come fare:
In Haskell, il logging può essere implementato utilizzando librerie come `monad-logger` o `hslogger`. Ecco un esempio rapido usando `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "Avvio dell'applicazione..."
    liftIO $ putStrLn "Esecuzione di un lavoro critico..."
    logErrorN "Oops! Qualcosa è andato storto."

main :: IO ()
main = runStdoutLoggingT logExample

{- Output di esempio
[Info] Avvio dell'applicazione...
Esecuzione di un lavoro critico...
[Error] Oops! Qualcosa è andato storto.
-}
```

Questo semplice esempio dimostra come puoi cospargere dichiarazioni di logging nel tuo codice per ottenere informazioni su ciò che sta accadendo durante l'esecuzione. `logInfoN` e `logErrorN` sono utilizzati per registrare rispettivamente messaggi informativi e di errore.

## Approfondimento:
Il logging è passato da semplici istruzioni di stampa a framework di logging sofisticati. Storicamente, i log erano solo output di testo su console o file, ma ora includono dati strutturati che possono essere analizzati e interpretati da vari strumenti.

In Haskell, il logging può essere fatto in uno stile puramente funzionale che implica il passaggio esplicito di azioni di log o l'uso di contesti monadici per l'impurità, dove i logger sono implicitamente passati attraverso il calcolo.

La libreria `hslogger`, per esempio, è più tradizionale e mutabile rispetto a `monad-logger`. `monad-logger` offre integrazione con lo stack di monadi e fornisce maggiore flessibilità in termini di formattazione dell'output e controllo. Entrambe le librerie ti permettono di impostare livelli di log, che aiutano a filtrare i messaggi di log in base alla loro importanza. I livelli di log includono debug, info, notice, warning, error, critical, alert e emergency.

L'approccio di Haskell al logging spesso si allinea con il suo enfasi sulla sicurezza dei tipi e sulla purezza. I log possono essere gestiti in modo che anche se il logging dovesse fallire, non causerà il crash dell'applicazione principale grazie alle robuste capacità di gestione degli errori di Haskell.

## Vedi anche:
- [documentazione di `monad-logger` su Hackage](https://hackage.haskell.org/package/monad-logger)
- [pacchetto `hslogger` su Hackage](https://hackage.haskell.org/package/hslogger)
- [Real World Haskell, Capitolo 19, sul gestione degli errori](http://book.realworldhaskell.org/read/error-handling.html)
- [La facciata di Logging per Haskell (log-base)](https://hackage.haskell.org/package/log-base)
