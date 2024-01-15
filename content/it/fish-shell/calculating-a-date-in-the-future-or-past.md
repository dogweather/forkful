---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Fish Shell: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

La ragione più comune per cui si vuole calcolare una data in futuro o in passato è la gestione delle scadenze. Potresti dover programmare un promemoria per un evento che si terrà in futuro o dover retrodatare un'azione che è stata completata.

## Come fare

Per calcolare una data in futuro o in passato nel Fish Shell, è possibile utilizzare la funzione `date` seguita dal numero di giorni, settimane, mesi o anni che si desidera aggiungere o sottrarre.

Esempio:

```Fish Shell
date "3 weeks ago"
```

Questo restituirà la data di tre settimane fa. È inoltre possibile combinare più unità di tempo in una sola espressione, ad esempio `3 days 2 hours ago`.

## Approfondimento

Oltre all'utilizzo della funzione `date`, è possibile anche utilizzare la data corrente come punto di partenza per calcolare una data in futuro o in passato. Ad esempio, se si è interessati alla data di 3 mesi fa a partire dalla data odierna, si può utilizzare questo comando:

```Fish Shell
date -s "3 months ago"
```

È inoltre possibile specificare una data di partenza diversa dalla data corrente utilizzando la funzione `date` seguita da un formato di data specifico. Ad esempio, `date 1992-03-08` restituirebbe la data 8 marzo 1992.

## Vedi anche

- [Documentazione ufficiale del Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial sul Fish Shell di DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-the-fish-shell-in-linux)
- [Calendario delle date e orari Unix](http://oreil.ly/unix-time-calendar)