---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:34:46.086576-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Leggere una data da una stringa significa capire e estrarre l'informazione temporale che la stringa contiene. I programmatori lo fanno per trasformare i dati in un formato che possono manipolare, confrontare, e analizzare.

## How to:
Per estrarre e manipolare date in Bash, puoi usare `date` e `GNU date`. Ecco come:

```Bash
stringa_data="2023-04-02 14:00:00"
# Converti la stringa in un timestamp Unix
timestamp=$(date -d "$stringa_data" +%s)

# Stampa la data in un formato personalizzato
echo $(date -d "@$timestamp" '+%d/%m/%Y %H:%M:%S')
```
Output:
```
02/04/2023 14:00:00
```

Puoi anche usare `awk` o `sed` per estrarre parti specifiche di una data.

```Bash
echo $stringa_data | awk '{print $1}'
```
Output:
```
2023-04-02
```

```Bash
echo $stringa_data | sed 's/ .*//'
```
Output:
```
2023-04-02
```

## Deep Dive:
Parsing delle date in Bash si appoggia a strumenti come `date` o GNU `date`. L'approccio con `date` offre flessibilità per manipolare e riformattare le date. Da notare che `date` ha diverse opzioni in sistemi Unix-like (Linux) rispetto a macOS, che usa BSD `date` e potrebbe richiedere alternative come `gdate` dopo aver installato `coreutils`.

In passato, i programmatori spesso scrivevano funzioni complesse in Bash o usavano linguaggi di scripting come Perl o Python per parsing di date più sofisticato. Oggi, `GNU date` è sufficientemente potente per la maggioranza dei casi.

Alternative include:

1. `awk` e `sed`: Ottimi per operazioni di stringa semplici.
2. `dateutils`: Una collezione di strumenti che aiutano con le date.
3. Linguaggi come Python o Perl: Utili se la logica del parsing diventa complessa.

Dettagli sull'implementazione variano. In Bash puro, controlli di formato e correzioni sono manuali. Utilizzando `GNU date`, si può specificare un formato e ottenere l'output desiderato tramite opzione `+FORMAT`.

## See Also:

- GNU Coreutils Manual: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
- Dateutils: http://www.fresse.org/dateutils/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/datesandtimes.html
