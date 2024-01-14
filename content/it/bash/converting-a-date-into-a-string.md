---
title:                "Bash: Conversione di una data in una stringa"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è una pratica comune nel mondo della programmazione, specialmente quando si lavora con dati temporali. Questa conversione può aiutare a visualizzare facilmente una data in un formato più facile da leggere o per elaborare ulteriormente i dati in una stringa.

## Come fare

Per convertire una data in una stringa utilizzando Bash, è necessario utilizzare il comando `date` seguito dal formato desiderato. Ad esempio, se si vuole convertire una data nel formato `anno-mese-giorno`, si può utilizzare il seguente codice:

```Bash
date +'%Y-%m-%d'
```

Il risultato dell'esecuzione di questo comando sarà una stringa contenente l'anno, il mese e il giorno correnti scritti nel seguente formato: `YYYY-MM-DD`.

Alcuni formati comuni utilizzati per visualizzare una data in una stringa sono:

- `%Y`: rappresenta l'anno
- `%m`: rappresenta il mese (numero)
- `%B`: rappresenta il mese (nome completo)
- `%d`: rappresenta il giorno
- `%A`: rappresenta il giorno della settimana (nome completo)

Per una lista completa dei formati disponibili, è possibile consultare la documentazione del comando `date`.

Un altro esempio pratico è la conversione di una data in un formato leggibile come `giorno-mese-anno`, utilizzando il seguente codice:

```Bash
date +'%d/%m/%Y'
```

Il risultato sarà una stringa contenente il giorno, il mese e l'anno correnti separati da "/" nel seguente formato: `DD/MM/YYYY`.

Utilizzando la combinazione di questi comandi e formati, è possibile creare diverse stringhe contenenti informazioni sulla data e sull'orario, utilizzando anche i formati per le ore, i minuti e i secondi.

## Approfondimento

La conversione di una data in una stringa può sembrare un'operazione semplice, ma può essere utile conoscere alcuni dettagli aggiuntivi per sfruttare al massimo questa funzionalità.

Per esempio, il comando `date` accetta anche l'opzione `-d` per specificare una data diversa dalla data corrente. Inoltre, il comando `date` può essere utilizzato anche per convertire una data in timestamp, ovvero un valore numerico che rappresenta il numero di secondi trascorsi dal 1 gennaio 1970.

Per convertire una data in un timestamp, è necessario utilizzare il formato `%s` seguito dalle opzioni `-d` e la data desiderata. Ad esempio:

```Bash
date +'%s' -d '2021-01-01'
```

Il risultato sarà un numero rappresentante il timestamp per la data specificata.

## Vedi anche

- Documentazione del comando `date`: https://www.gnu.org/software/coreutils/date
- Lista dei formati disponibili per il comando `date`: https://www.gnu.org/software/coreutils/manual/html_node/Date-conversion-specifiers.html
- Esempi di utilizzo del comando `date`: https://www.geeksforgeeks.org/date-command-linux-examples/