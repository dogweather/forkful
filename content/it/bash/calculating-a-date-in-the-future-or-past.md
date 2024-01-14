---
title:                "Bash: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o passato è una delle basi della programmazione di Bash. Con questa funzionalità, è possibile automatizzare la gestione del tempo in numerosi script e programmi. 

## Come fare

Per calcolare una data nel futuro o passato, si può utilizzare il comando "date" di Bash. Di seguito un esempio pratico:

```Bash
# Calcolare la data tra 30 giorni
future_date=$(date +%Y-%m-%d -d "30 days") 
echo "La data tra 30 giorni sarà: $future_date"
```

Questo comando utilizza l'opzione "-d" per specificare il numero di giorni nel futuro o passato. In questo caso, abbiamo utilizzato "30 days" per ottenere una data tra 30 giorni. È possibile utilizzare diverse unità di tempo come "weeks", "months" o "years".

È anche possibile specificare una data di riferimento utilizzando l'opzione "-d". Ad esempio:

```Bash
# Calcolare la data tra 2 settimane a partire dall'1 gennaio 2022
future_date=$(date +%Y-%m-%d -d "2 weeks 2022-01-01") 
echo "La data tra 2 settimane a partire dall'1 gennaio 2022 sarà: $future_date"
```

In questo modo, è possibile ottenere una data basata su una data specifica invece che sulla data corrente.

## Approfondimento

Il comando "date" utilizza un formato specifico per visualizzare la data. In questo esempio, abbiamo utilizzato "%Y-%m-%d" per mostrare la data nel formato "anno-mese-giorno". Tuttavia, è possibile utilizzare diversi formati per adattare l'output alle proprie esigenze. 

Inoltre, è possibile utilizzare il comando "date" anche per calcolare una data precedente, utilizzando un numero di giorni negativo. Ad esempio:

```Bash
# Calcolare la data di 45 giorni fa
past_date=$(date +%Y-%m-%d -d "-45 days") 
echo "La data di 45 giorni fa è: $past_date"
```

Questo comando sarà utile per gestire anche le date passate nei propri script e programmi.

## Vedi anche

- https://www.computerhope.com/unix/udate.htm
- https://www.shellhacks.com/it/date-command-examples-linux-unix-bash-scripts/
- https://man7.org/linux/man-pages/man1/date.1.html