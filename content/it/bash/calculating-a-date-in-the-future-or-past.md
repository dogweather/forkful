---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Bash: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Calcolare una data futura o passata è il processo di determinazione di date che sono a una certa distanza temporale da una data di riferimento. I programmatori lo fanno per gestire eventi programmati o per tracciare l'età di specifici dati.

## Come fare:

Ecco come calcolare una data futura o passata in Bash. Puoi usare il comando `date` con l'opzione `-d` (o `--date`).

```Bash
# Per calcolare una data in futuro di 10 giorni
date -d "+10 days"

# Per calcolare una data nel passato di 5 giorni
date -d "-5 days"
```
L'output sarà la data calcolata nel formato predefinito.

## Approfondimento

Storicamente, calcolare una data futura o passata non era affatto semplice. Prima della disponibilità del comando `date`, i programmatori dovevano scrivere complesse funzioni per gestire queste operazioni.

Ci sono alternative al comando `date`, come il comando `strtotime` in PHP che può gestire una varietà di stringhe di data e ora.

Il calcolo di una data futura o passata con il comando `date` funziona semplicemente aggiungendo o sottraendo il numero specificato di giorni dalla data corrente.

## Vedi Anche

1. Manuale di Bash: https://www.gnu.org/software/bash/manual/bash.html
2. Comando `date`: https://man7.org/linux/man-pages/man1/date.1.html
3. Comando `strtotime` di PHP: https://www.php.net/manual/en/function.strtotime.php