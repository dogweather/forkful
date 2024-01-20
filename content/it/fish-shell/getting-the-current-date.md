---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?

Ottenere la data corrente significa accedere al giorno, mese e anno correnti del sistema. I programmatori lo fanno per tracciare eventi, registrare timestamp o schedulare attività.

## Come fare:

Ecco un esempio di come ottenere la data corrente in Fish Shell:

```Fish Shell
date
```

Questo vi mostrerà un output simile a:

```Fish Shell
Tue Sep 21 14:19:18 CEST 2021
```

Per ottenere solo la data, senza orario, potete usare:

```Fish Shell
date '+%d-%m-%Y'
```

Esempio di output:

```Fish Shell
21-09-2021
```

## Approfondimento:

Nel contesto storico, i primi sistemi informatici non avevano un modo integrato per ottenere la data corrente. Questo era spesso fatto manualmente o con script personalizzati.

In quanto alla Fish shell, questo è implementato usando internal commands, non esterne, per garantire che funzioni su qualsiasi sistema, non solo su quelli che hanno certi programmi installati.

Alcune alternative potrebbero includere il comando `printf` con il formato `%T` per ottenere solo l'orario, o il comando `date +%s` per ottenere la data in formato Unix epoch.

## Vedi anche:

Per informazioni più dettagliate, visitate:

- Documentazione ufficiale di Fish Shell: [fishshell.com/docs/current](https://fishshell.com/docs/current/)
- Guida agli Shell Scripts in Linux: [linuxconfig.org/bash-scripting-tutorial-for-beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)