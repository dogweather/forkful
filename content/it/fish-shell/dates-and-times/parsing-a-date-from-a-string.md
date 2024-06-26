---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:06.096662-07:00
description: "Come fare: In Fish Shell, non si dispone di comandi integrati specificamente\
  \ progettati per l'analisi delle date da stringhe. Invece, ci si affida a\u2026"
lastmod: '2024-03-13T22:44:43.868928-06:00'
model: gpt-4-0125-preview
summary: In Fish Shell, non si dispone di comandi integrati specificamente progettati
  per l'analisi delle date da stringhe.
title: Analisi di una data da una stringa
weight: 30
---

## Come fare:
In Fish Shell, non si dispone di comandi integrati specificamente progettati per l'analisi delle date da stringhe. Invece, ci si affida a utility esterne come `date` (disponibile su Linux e macOS) o si sfruttano strumenti di terze parti popolari come `GNU date` per analisi più complesse. Ecco come procedere:

**Usare `date` con Fish:**

Per analizzare una stringa di data nel formato "YYYY-MM-DD", è possibile utilizzare il comando `date` con l'opzione `-d` (o `--date` per GNU date) seguita dalla stringa. L'opzione `+` viene utilizzata per formattare l'output.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# Output: Sabato, 01 Aprile 2023
```

Per macOS (che richiede un formato diverso per le flag `-j` e `-f`):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# Output: Sabato, 01 Aprile 2023
```

**Usare GNU `date` per analisi complesse:**

GNU `date` è più flessibile con i formati delle stringhe. Può rilevare automaticamente molti formati comuni di stringhe di date senza specificare esplicitamente il formato di input:

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# Output: 2023-04-01 14:00:00
```

Tuttavia, quando si lavora con stringhe di date che potrebbero non essere riconosciute automaticamente o quando è necessario un controllo preciso sul formato di input, specificare il formato di input con GNU `date` non è direttamente supportato. In tali casi, prendere in considerazione la preelaborazione della stringa o l'utilizzo di un altro strumento progettato per routine di analisi di date più complesse.
