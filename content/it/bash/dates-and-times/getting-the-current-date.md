---
title:                "Ottenere la data corrente"
aliases:
- /it/bash/getting-the-current-date/
date:                  2024-02-03T19:08:50.831419-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ottenere la data corrente"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è e perché?
Il recupero della data corrente in Bash comporta l'utilizzo di comandi integrati per visualizzare la data e l'ora in vari formati. I programmatori utilizzano questa funzionalità per compiti come la marcatura temporale dei log, la pianificazione delle attività, o semplicemente come parte dei loro script di informazioni di sistema per tracciare quando sono state eseguite delle azioni.

## Come fare:
In Bash, il comando `date` è il tuo strumento principale per ottenere la data e l'ora correnti. Ecco alcuni esempi di come usarlo:

1. **Ottenere la data e l'ora correnti nel formato predefinito:**

```bash
date
```

*Output di esempio:*
```
Mer Apr 5 14:22:04 PDT 2023
```

2. **Personalizzare il formato dell'output:** Puoi specificare il formato dell'output utilizzando i formati specificatori `+%`. Per esempio, per visualizzare la data nel formato AAAA-MM-GG:

```bash
date "+%Y-%m-%d"
```

*Output di esempio:*
```
2023-04-05
```

3. **Ottenere il timestamp UNIX corrente:** Il timestamp UNIX è il numero di secondi dal Epoch Unix (1 Gennaio 1970). Questo è utile per gli script che eseguono calcoli basati sulle differenze temporali.

```bash
date "+%s"
```

*Output di esempio:*
```
1672877344
```

Nessuna libreria di terze parti popolare è tipicamente utilizzata per questa operazione basilare in Bash poiché il comando `date` integrato fornisce una funzionalità completa. Tuttavia, per manipolazioni di date e orari più avanzate, i programmatori potrebbero utilizzare altri linguaggi di programmazione o strumenti che offrono librerie per l'aritmetica e l'analisi della data, come il modulo `datetime` di Python.
