---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:12:58.138628-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"

category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente in Bash è come chiedersi “Che giorno è oggi?”. È fondamentale in scripting per log, deadlines, e per automatizzare compiti legati al tempo.

## How to:
Ottenere la data e l'ora attuali è semplice:

```Bash
date
```

Output potrebbe essere:

```
mar apr  4 12:34:56 CEST 2023
```

Per un formato personalizzato, usiamo `+` con specificatori di formato:

```Bash
date "+%d/%m/%Y %H:%M:%S"
```

Output:

```
04/04/2023 12:34:56
```

## Deep Dive
La funzione `date` è usata da quando le shell su Unix-like systems esistono. È versatile e si appoggia al sistema per l'orario. Gli script possono usarlo per marcare eventi (timestamps), per timeout, e nelle operazioni cron.

Alternatives? `date` non è l'unica via. Potresti usare `awk` o `perl` per giocare con l'orario, ma perché complicarsi la vita?

Dettagli di implementazione: `date` si interfaccia con il sistema per rilevare la zona oraria e l'aggiustamento di ora legale, se applicabile. Usare formati specifici rende lo script adattabile e leggibile, ma ricorda di considerare la localizzazione!

## See Also
- Manuale Bash: `man date`
- Specificatori di formato: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Concetti di scripting con date e orari: https://www.tldp.org/LDP/abs/html/timedate.html
