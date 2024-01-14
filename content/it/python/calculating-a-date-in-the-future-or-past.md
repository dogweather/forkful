---
title:                "Python: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Calcolare una data nel futuro o nel passato è un'operazione comune in molti ambiti della programmazione, specialmente quando si lavora con dati che sono legati al tempo. Può essere utile per eseguire operazioni come previsioni, gestire scadenze o pianificare eventi.

## Come fare
Per calcolare una data in Python, possiamo utilizzare il modulo `datetime`. Questo modulo ci permette di lavorare con oggetti `datetime` e di eseguire operazioni come il calcolo della differenza tra due date o la conversione di una stringa in un oggetto `datetime`.

```python
import datetime

# Definiamo una data di partenza 
start_date = datetime.datetime(2021, 12, 15)

# Calcoliamo una data nel futuro aggiungendo 30 giorni alla data di partenza
future_date = start_date + datetime.timedelta(days=30)

# Calcoliamo una data nel passato sottraendo 7 giorni alla data di partenza
past_date = start_date - datetime.timedelta(days=7)

# Stampiamo i risultati
print(f"Data di partenza: {start_date}")
print(f"Data nel futuro: {future_date}")
print(f"Data nel passato: {past_date}")
```
Output:
```
Data di partenza: 2021-12-15 00:00:00
Data nel futuro: 2022-01-14 00:00:00
Data nel passato: 2021-12-08 00:00:00
```

## Approfondimento
Per ottenere una maggiore precisione nel calcolo di una data nel futuro o nel passato, possiamo utilizzare il modulo `dateutil`. Questo modulo permette di gestire anche la differenza tra date che includono anche i valori delle ore, dei minuti e dei secondi.

```python
from dateutil.relativedelta import relativedelta

# Definiamo una data di partenza 
start_date = datetime.datetime(2021, 12, 15)

# Calcoliamo una data nel futuro aggiungendo 30 giorni, 6 ore, 30 minuti e 15 secondi alla data di partenza
future_date = start_date + relativedelta(days=30, hours=6, minutes=30,seconds=15)

# Calcoliamo una data nel passato sottraendo 1 anno, 2 mesi e 10 giorni alla data di partenza
past_date = start_date - relativedelta(years=1, months=2, days=10)

# Stampiamo i risultati
print(f"Data di partenza: {start_date}")
print(f"Data nel futuro: {future_date}")
print(f"Data nel passato: {past_date}")
```
Output:
```
Data di partenza: 2021-12-15 00:00:00
Data nel futuro: 2022-01-14 06:30:15
Data nel passato: 2020-10-05 00:00:00
```

## Vedi anche
- [Documentazione ufficiale di Python per il modulo `datetime`](https://docs.python.org/3/library/datetime.html)
- [Documentazione ufficiale di Python per il modulo `dateutil`](https://dateutil.readthedocs.io/en/stable/index.html)
- [Tutorial su come calcolare date nel futuro o nel passato con Python](https://realpython.com/python-datetime/)