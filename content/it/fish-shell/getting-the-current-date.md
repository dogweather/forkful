---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:14:20.392844-07:00
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Ottenere la data corrente è un'operazione che ti dice quale giorno è oggi nel formato che preferisci. I programmatori la usano per log, scadenze, e funzioni temporali nelle loro applicazioni.

## Come fare:
Ecco come ottenere la data corrente in Fish Shell:

```
Fish Shell
set oggi (date "+%Y-%m-%d")
echo $oggi
```

Output:
```
2023-04-01
```

Per visualizzare tempo e data:

```
Fish Shell
set ora_esatta (date "+%Y-%m-%d %H:%M:%S")
echo $ora_esatta
```

Output:
```
2023-04-01 15:45:30
```

## Approfondimento:
Il comando `date` esiste da decenni, parte essenziale dei sistemi Unix-like per ottenere o impostare data e ora. In Fish, a differenza di altri shell, non serve esportare la variabile (`set` basta).

Alternative includono l'uso di `strftime` all'interno di script o funzioni di Fish per avere più controllo sul formato della data.

Dettagli di implementazione: Fish Shell esegue comandi esterni, come `date`, in un modo che è coerente con la sua filosofia semplice e intuitiva. Usa `set` per salvare il risultato del comando `date` in una variabile che può essere facilmente riutilizzata.

## Vedere anche:
- La documentazione ufficiale di Fish Shell: [fishshell.com](https://fishshell.com/docs/current/index.html)
- Informazioni sul comando `date` su man page: esegui `man date` nel terminale o visita [man7.org](https://man7.org/linux/man-pages/man1/date.1.html)
- Una guida su `strftime` per formati di data personalizzati: [strftime.org](https://strftime.org/)
