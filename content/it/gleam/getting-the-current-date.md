---
title:                "Gleam: Ottenere la data corrente"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui si potrebbe voler ottenere la data corrente in un programma Gleam. Ad esempio, potresti voler utilizzare la data per calcolare delle scadenze, creare dei log, o semplicemente mostrare la data nel formato desiderato.

## Come

Per ottenere la data corrente in Gleam, esistono due modi principali: utilizzando le funzioni `Date.now` o `Date.utc_now`.

```
Gleam
import Date

// Ottieni la data corrente come timestamp UTC
let currentUtcTimestamp = Date.utc_now()

// Oppure, ottieni la data corrente come un record con più informazioni utili
let currentDate = Date.now()
```

L'output di `currentUtcTimestamp` sarà un intero rappresentante il numero di millisecondi trascorsi dal 1 gennaio 1970, mentre `currentDate` sarà un record contenente il giorno, il mese, l'anno, l'ora, il minuto, il secondo e il millisecondo correnti.

## Deep Dive

Se vuoi saperne di più sul funzionamento interno delle funzioni `Date.now` e `Date.utc_now`, puoi consultarne la documentazione ufficiale di Gleam. Inoltre, è possibile utilizzare queste funzioni per ottenere la data in vari formati, utilizzando le funzioni di formattazione della libreria standard o scrivendone una personalizzata.

## Vedi anche

- Documentazione ufficiale di Gleam per Date: https://gleam.run/documentation/stdlib/date
- Funzioni di formattazione della libreria standard di Gleam: https://gleam.run/documentation/stdlib/format