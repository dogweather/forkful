---
title:                "Convertire una data in una stringa"
html_title:           "Gleam: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore, probabilmente hai già incontrato il problema di dover convertire una data in una stringa. Ci sono molte ragioni per cui potresti dover fare ciò, ad esempio la necessità di visualizzare la data in un formato specifico o di utilizzare la stringa come parte di un URL. Indipendentemente dal motivo, in questo articolo ti mostrerò come convertire una data in una stringa utilizzando il linguaggio di programmazione Gleam.

## Come fare

Per prima cosa, dovrai importare l'API DateTime di Gleam nel tuo progetto. Puoi farlo utilizzando il seguente codice:

```Gleam
import gleam/datetime
```

Una volta importata l'API DateTime, puoi utilizzare la funzione `DateTime.format` per convertire una data in una stringa. Vediamo un esempio:

```Gleam
let date = DateTime.create(~year=2021, ~month=5, ~day=15)
let string = DateTime.format(date, "%d/%m/%Y")
```

In questo esempio, abbiamo creato una data con l'API DateTime, specificando l'anno, il mese e il giorno. Abbiamo poi utilizzato la funzione `format` per convertire la data in una stringa nel formato giorno/mese/anno. Se stampiamo la variabile `string`, otterremo il seguente output:

```Gleam
15/05/2021
```

Puoi provare diversi formati utilizzando diverse combinazioni di caratteri. Ad esempio, utilizzando `%a, %d %b %Y` otterremo il seguente output:

```Gleam
sabato, 15 maggio 2021
```

Oltre a convertire una data in una stringa, è possibile anche fare il contrario: convertire una stringa in una data utilizzando la funzione `DateTime.parse`. Questa funzione richiede due parametri: la stringa da convertire e il formato della data.

## Approfondimenti

Se vuoi saperne di più sulle funzionalità della funzione `DateTime.format`, ti consiglio di dare un'occhiata alla documentazione ufficiale di Gleam. Qui troverai una lista completa di tutte le combinazioni possibili di caratteri per formattare una data.

Inoltre, è importante tenere conto delle differenze tra i formati delle date in diversi paesi o lingue. Assicurati di considerare questi aspetti quando utilizzi la funzione `format` o `parse` per evitare problemi con la formattazione della data.

## Vedi anche

- [Documentazione ufficiale di Gleam su DateTime](https://gleam.run/modules/gleam/datetime.html)
- [Tutorial di Gleam su date e ore](https://gleam.run/blog/date-time.html)
- [API DateTime di Erlang (su cui si basa Gleam)](http://erlang.org/doc/man/calendar.html#strftime-3)