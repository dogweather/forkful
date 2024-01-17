---
title:                "Confrontare due date"
html_title:           "Bash: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?:
Confrontare due date è un'operazione comune per i programmatori, che permette di determinare se una data è precedente, successiva o uguale a un'altra. Questa funzione è utile in molte situazioni, come ad esempio nell'ordinamento di dati o nella gestione di scadenze.

## Come fare:
Per confrontare due date in Bash, è possibile utilizzare il comando `date` seguito dalle opzioni `+%s` (formato epoch in secondi) e `-d` (per indicare la data da confrontare). Di seguito un esempio di codice e output:

```Bash
date +%s -d "12 July 2020"
1594521600
date +%s -d "15 July 2020"
1594761600
```
In questo esempio, il comando `date` converte le due date in formati epoch (secondi), che possono poi essere confrontati per determinare quale data sia precedente o successiva.

## Approfondimento:
Nelle prime versioni di Unix, il formato epoch era espresso in secondi dal 1 gennaio 1970. Invece, a partire dal 2038, il formato epoch utilizzerà 64 bit al posto dei 32 bit attuali, permettendo un utilizzo della funzione di comparazione delle date anche oltre il 2038.

Una possibile alternativa per confrontare due date in Bash è utilizzare il comando `expr`, che permette di calcolare differenze in modo più preciso. È anche possibile utilizzare altre soluzioni esterne, come ad esempio l'uso di script in Python o l'installazione di tool specifici per gestire le date.

Per implementare una funzione di confronto delle date in Bash, è importante considerare eventuali formati diversi delle date inserite dall'utente, utilizzando ad esempio il comando `sed` per effettuare una formattazione corretta prima della comparazione.

## Vedi anche:
- Documentazione ufficiale di `date` in Bash: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Approfondimenti sul formato epoch: https://en.wikipedia.org/wiki/Unix_time
- Ulteriori esempi di confronto di date in Bash: https://www.baeldung.com/linux/compare-dates-bash