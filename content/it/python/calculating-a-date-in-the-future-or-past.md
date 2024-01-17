---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Python: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa & Perché? 

Calcolare una data nel futuro o nel passato è un'operazione comune nella programmazione. Viene utilizzata per eseguire operazioni come pianificare eventi futuri, calcolare scadenze e analizzare dati storici.

I programmatori eseguono queste operazioni per automatizzare compiti ripetitivi e per creare applicazioni più efficienti e precise.

## Come fare: 

Ecco un semplice esempio di codice in Python che calcola la data odierna più 7 giorni nel futuro:

```Python
from datetime import date, timedelta
future_date = date.today() + timedelta(days=7)
print(future_date)
```

Output: 2020-11-17

Per calcolare una data passata, basta sostituire l'operatore "+" con "-":

```Python
from datetime import date, timedelta
past_date = date.today() - timedelta(days=7)
print(past_date)
```

Output: 2020-11-03 

## Approfondimento: 

Il calcolo di date è stato un problema fin dai primi anni della programmazione. Prima della disponibilità di librerie e funzioni specifiche, i programmatori dovevano utilizzare algoritmi complicati per ottenere date future o passate.

Oltre a timedelta, Python offre anche la libreria "datetime" che permette il calcolo di date utilizzando altri parametri come mese, anno e ore.

In alternativa, ci sono anche librerie esterne come "arrow" e "pendulum" che offrono funzioni più avanzate per il calcolo di date.

Il calcolo di date è anche un aspetto importante del machine learning, in cui viene utilizzato per analizzare e prevedere trend futuri basati su dati storici.

## Vedi anche: 

- [Documentazione ufficiale di Python](https://docs.python.org/3/library/datetime.html)
- [Libreria "datetime" di Python](https://docs.python.org/3/library/datetime.html#datetime-objects)
- [Libreria "arrow" per il calcolo di date](https://arrow.readthedocs.io/en/latest/)
- [Libreria "pendulum" per il calcolo di date](https://pendulum.eustace.io/)