---
title:    "Gleam: Calcolare una data nel futuro o nel passato"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potrebbe essere necessario calcolare una data nel futuro o nel passato, come pianificare eventi, tenere traccia delle scadenze o fare previsioni finanziarie. Con l'uso di Gleam, possiamo automatizzare questi calcoli per semplificare il nostro lavoro.

## Come fare

Per fare questo, possiamo utilizzare la funzione ```Gleam.Time.add_days()``` in combinazione con ```Gleam.Time.format()```. Ad esempio, per calcolare la data tra 7 giorni dalla data corrente, possiamo utilizzare il codice seguente:

```
let data = Gleam.Time.now()
let data_futura = Gleam.Time.add_days(data, 7)
let output = Gleam.Time.format(data_futura, "yyyy-mm-dd")
```

Il risultato sarà una stringa contenente la data di 7 giorni nel futuro, nel formato "anno-mese-giorno".

## Approfondimento

È possibile utilizzare anche altre funzioni come ```Gleam.Time.add_months()``` o ```Gleam.Time.add_years()``` per calcolare date in futuro o passato a lungo termine. Inoltre, Gleam offre anche la possibilità di specificare il fuso orario o la lingua nella formattazione della data.

Inoltre, se dobbiamo effettuare calcoli più complessi sulle date, possiamo utilizzare la libreria ```Gleam.Date```, che offre una vasta gamma di funzioni per lavorare con le date, come il calcolo del giorno della settimana o la conversione tra differenti formati di data.

## Vedi anche

- [Documentazione di Gleam sull'utilizzo delle date](https://gleam.run/modules/gleam_time.html)
- [Esempi pratici di utilizzo delle funzioni Gleam.Time](https://github.com/gleam-lang/examples/blob/master/time/date_range.gleam)
- [Gleam Date - libreria aggiuntiva per lavorare con le date](https://github.com/lpil/gleam-date)