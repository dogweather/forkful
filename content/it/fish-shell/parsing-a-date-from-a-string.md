---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:36:04.051758-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Tradurre una stringa in una data significa estrarre informazione temporale da un testo. Si fa per manipolare e confrontare date in modo più semplice.

## How to:
```Fish Shell
set data_string "2023-03-15 10:30:00"
set timestamp (date -d $data_string "+%s")
echo $timestamp
```
Output:
```
1678871400
```

```Fish Shell
set data_leggibile (date -d "2023-03-15 10:30:00" "+%A, %d %B %Y %H:%M:%S")
echo $data_leggibile
```
Output:
```
Wednesday, 15 March 2023 10:30:00
```

## Deep Dive
Historia: Il parsing delle date è essenziale sin dall'inizio della programmazione. Con l'evoluzione dei sistemi, le funzioni di parsing sono diventate più robuste.

Alternative: Oltre a `date`, ci sono strumenti come `strptime` e librerie in vari linguaggi per parsing più complessi.

Dettagli Implementativi: `date` in Fish (e Unix in generale) utilizza formati di data standard, `%s` per timestamp e altri identificatori, come `%A`, `%d`, per formati leggibili.

## See Also
- Tutorial sui comandi `date`: https://ss64.com/bash/date.html
- Documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/index.html
- Documentazione POSIX `strptime`: https://pubs.opengroup.org/onlinepubs/9699919799/functions/strptime.html