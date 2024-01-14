---
title:    "Gleam: Convertire una data in una stringa"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore o hai interesse ad apprendere il linguaggio di programmazione Gleam, potresti chiederti come convertire una data in una stringa. La risposta è semplice: spesso è necessario trasformare una data in una stringa per poterla visualizzare o manipolarla all'interno di un programma. In questo articolo, esploreremo come farlo utilizzando Gleam.

## Come fare
Per convertire una data in una stringa in Gleam, dobbiamo prima di tutto creare una data utilizzando la libreria standard `Time.Date`. Ad esempio:

```Gleam
let data = Time.Date.new (year = 2020, month = 12, day = 31)
```

Una volta creata la data, possiamo utilizzare la funzione `format` per convertirla in una stringa, specificando un formato desiderato. Ad esempio, per ottenere una stringa nel formato "DD/MM/YYYY", possiamo utilizzare il seguente codice:

```Gleam
let stringa = Time.Date.format({data, format = "%d/%m/%Y"})
```

L'output di questo codice sarà `31/12/2020`, che è esattamente ciò che volevamo.

## Approfondimento
La funzione `format` accetta diversi parametri per ottenere il formato desiderato della stringa. Ad esempio, possiamo specificare l'uso di ore, minuti e secondi nella stringa finale se li vogliamo inclusi. Inoltre, possiamo anche specificare l'uso di abbreviazioni per i mesi o i giorni della settimana. La libreria `Time.Date` ha molte altre funzioni utili che possono essere utilizzate per manipolare le date e le stringhe in modo efficiente.

## Vedi anche
- [Documentazione ufficiale di Gleam su Time.Date](https://gleam.run/modules/time-date/)
- [Esempi pratici di utilizzo di Time.Date](https://gist.github.com/gleamrun/944c7873e862ffc43e53064f82be008b)
- [Impara a programmare in Gleam](https://gleam.run/learn/)