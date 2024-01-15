---
title:                "Ottenere la data corrente."
html_title:           "Fish Shell: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Se ti stai chiedendo perché dovresti essere interessato/a a ottenere la data corrente in Fish Shell, probabilmente stai cercando un modo per automatizzare una qualche attività basata sulla data, come ad esempio creare una cartella o un file con la data corrente. Ottenere la data corrente può sembrare un dettaglio insignificante, ma può essere estremamente utile in alcune situazioni.

## Come fare

Per ottenere la data corrente utilizzando Fish Shell, puoi utilizzare il comando `date`, seguito dalle opzioni necessarie. Ad esempio, se vuoi ottenere la data nel formato `gg/mm/aaaa`, puoi utilizzare il seguente comando:

```
Fish Shell get current date in Italian
```

L'output sarà qualcosa del tipo `25/11/2021`, che rappresenta la data odierna. Puoi personalizzare il formato della data utilizzando le opzioni del comando `date`. Ad esempio, se vuoi mostrare anche il giorno della settimana, puoi utilizzare l'opzione `-u`:

```
Fish Shell get current date with day of the week in Italian
```

L'output sarà qualcosa del tipo `gio 25 nov 2021`, dove `gio` rappresenta il giorno della settimana abbreviato.

## Approfondimento

Se sei interessato/a a comprendere meglio come funziona il comando `date`, puoi fare riferimento alla sua pagina di manuale utilizzando il comando `man date` nel terminale. Troverai informazioni dettagliate sulle opzioni disponibili e sui formati che puoi utilizzare per ottenere la data corrente esattamente come desideri.

## Vedi anche

- Documentazione ufficiale del comando `date`: https://fishshell.com/docs/current/commands/date.html
- Tutte le opzioni del comando `date` e i relativi formati di output: https://fishshell.com/docs/current/commands/date.html#option-quickref