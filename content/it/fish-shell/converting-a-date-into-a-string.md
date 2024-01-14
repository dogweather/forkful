---
title:                "Fish Shell: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si trovano spesso nella situazione di dover convertire una data in una stringa. Questo può essere utile per molteplici motivi, ad esempio per stampare una data in un formato specifico o per confrontare date in un algoritmo di ordinamento.

## Come fare

Per convertire una data in una stringa utilizzando Fish Shell, possiamo utilizzare il comando `date` combinato con le opzioni `+%Y`, `+%m`, `+%d` per ottenere rispettivamente l'anno, il mese e il giorno della data. Ad esempio:

```
Fish Shell
$ date +%Y
2021
```

Possiamo quindi concatenare queste informazioni per ottenere una stringa nella forma desiderata. Esempio di codice completo:

```
Fish Shell
$ set year (date +%Y)
$ set month (date +%m)
$ set day (date +%d)
$ echo "$day/$month/$year"
07/11/2021
```

## Approfondimento

Per effettuare una conversione più avanzata, possiamo utilizzare il comando `strftime` che ci consente di specificare un formato di data personalizzato. Ad esempio, se vogliamo ottenere la data nel formato "GG/MM/AAAA" possiamo utilizzare il seguente codice:

```
Fish Shell
$ strftime "%d/%m/%Y" (date)
07/11/2021
```

Possiamo anche applicare operazioni matematiche alla data prima di convertirla in una stringa. Per esempio, se vogliamo ottenere la data di domani:

```
Fish Shell
$ strftime "%d/%m/%Y" (date -d "+1 day")
08/11/2021
```

## Vedi anche

- Documentazione di Fish Shell sulla conversione di date: https://fishshell.com/docs/current/cmds/date.html
- Spiegazione dettagliata sull'utilizzo del comando `strftime`: https://www.cyberciti.biz/faq/unix-linux-date-formatting-syntax/
- Esempi di codice per convertire date in stringhe in altri linguaggi di programmazione: https://www.techiedelight.com/convert-date-into-string-various-programming-languages/