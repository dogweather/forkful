---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Gleam: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Sebbene la maggior parte dei programmi utilizzino date attuali, potrebbe essere necessario calcolare date in futuro o in passato per svolgere specifiche attività come tracciare scadenze o gestire eventi programmati. In questo articolo, impareremo come utilizzare Gleam per eseguire questo tipo di calcoli.

## Come Fare
Per calcolare una data futura o passata, possiamo utilizzare la funzione `Calendar.advance` che accetta come argomenti la data di partenza e la quantità di tempo da avanzare (positivo per date future, negativo per date passate) e restituisce la data risultante. Ecco un esempio di codice:

```
Gleam> Calendar.advance(#year(2021), #day(2), #month(7), 10)
#date(2021, 7, 12) 
```

In questo esempio, abbiamo calcolato la data risultante di avanzare di 10 giorni a partire dal 2 luglio 2021. Possiamo anche specificare un tempo più preciso, ad esempio avanzare di 2 anni, 7 mesi e 10 giorni:

```
Gleam> Calendar.advance(#datetime(#date(2021, 7, 2), #time(10, 30, 0)), #days(2), #months(7), #years(2))
#datetime(#date(2023, 2, 4), #time(10, 30, 0))
```

In questo caso, abbiamo utilizzato la funzione `Calendar.datetime` per specificare sia la data che l'orario di partenza.

## Approfondimento
Calcolare date in futuro o in passato può risultare utile anche nel caso di gestione di fusi orari o durante l'analisi di dati storici. In questi casi, è importante prestare attenzione alle differenze nei formati delle date e ai possibili errori di calcolo dovuti alla presenza di anni bisestili.

## Vedi Anche
- Documentazione ufficiale di Gleam sulla gestione delle date: https://gleam.run/documentation//current/lib/calendar.html
- Esempi di utilizzo di Gleam per il calcolo di date in futuro e in passato: https://github.com/gleam-lang/gleam-examples/tree/master/dates