---
title:                "Ottenere la data corrente"
html_title:           "Bash: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è e perché lo si fa?

Nella programmazione, ottenere la data corrente è un'operazione comune e utile. Consiste nell'acquisire la data e l'ora attuali del sistema in cui si sta eseguendo il codice. I programmatori lo fanno per diverse ragioni, ad esempio per registrare gli orari di esecuzione dei processi, per organizzare i file in base alla data o per visualizzare la data ai fini di debugging.

## Come si fa?

Per ottenere la data corrente in Bash, è possibile utilizzare il comando ```date```. Ad esempio, per visualizzare la data attuale nel formato "gg/mm/aaaa", si può digitare nella shell:

```Bash
date +%d/%m/%Y
```
L'output sarà qualcosa del tipo: ```20/08/2021```.

Per visualizzare anche l'ora o il fuso orario, è possibile utilizzare delle opzioni aggiuntive come ad esempio ```+%T``` per l'ora in formato "hh:mm:ss" o ```+%Z``` per il fuso orario. Un esempio completo potrebbe essere:

```Bash
date +%d/%m/%Y %T %Z
```

E l'output potrebbe essere: ```20/08/2021 09:24:12 EDT```

## Spiegazione più approfondita

Il comando ```date``` esiste anche in altri sistemi operativi come Unix e Linux, e può essere usato per cambiare la data e l'ora del sistema. In alternativa, esistono anche altri comandi per ottenere la data corrente, come ad esempio ```now()``` in Python e ```LocalDate.now()``` in Java.

Per quanto riguarda l'implementazione, il comando ```date``` utilizza la libreria C standard per accedere alla data del sistema e la formatta secondo le specifiche fornite dall'utente. Questo rende il comando molto versatile e personalizzabile.

## Vedi anche

- [Documentazione ufficiale del comando date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Altri modi per ottenere la data corrente in Bash](https://stackoverflow.com/questions/1401482/yyyy-mm-dd-format-date-in-shell-script)
- [Informazioni sulla libreria C standard](https://www.geeksforgeeks.org/c-standard-library-header-files/)