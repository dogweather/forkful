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

# Calcolare una Data nel Futuro o nel Passato con Python

## Che Cosa & Perché?
Calcolare una data nel futuro o nel passato significa cercare una data specifica rispetto a un punto di riferimento. I programmatori lo fanno per gestire eventi che si verificano in un momento diverso dal presente, come la programmazione di promemoria o la misura del tempo trascorso.

## Come Fare:
In Python, possiamo sfruttare il modulo `datetime` per calcolare date future o passate. Ecco un esempio:

```python
from datetime import datetime, timedelta

# Data corrente
oggi = datetime.now()

# Aggiungere 5 giorni alla data corrente
futuro = oggi + timedelta(days=5)
print("Data nel futuro: ", futuro)

# Sottrarre 5 giorni alla data corrente
passato = oggi - timedelta(days=5)
print("Data nel passato: ", passato)
```
Se esegui questo codice, otterrai un output simile a:

```python
Data nel futuro:  2022-12-10 18:29:14.233953
Data nel passato: 2022-11-30 18:29:14.233953
```

## Approfondimento
Anche se Python non esisteva nell'era pre-digital, i concetti di calcolo delle date sono esistiti fin dall'antichità, sia per pianificare eventi futuri che per registrare quelli passati.

Sebbene `datetime` sia la scelta più diffusa per la gestione delle date in Python, esistono alternative come `Pendulum`, `Maya` o `Arrow`, che forniscono un'API più intuitiva o funzionalità extra.

Il calcolo di una data nel futuro o nel passato con `datetime` implica la creazione di un oggetto `datetime` per la data corrente, quindi l'aggiunta o la sottrazione di un oggetto `timedelta`, che rappresenta una durata.

## Vedi Anche
1. [Python `datetime` Documentazione Ufficiale](https://docs.python.org/3/library/datetime.html)
2. [Gestione delle date e delle ore in Python con Pendulum](https://pendulum.eustace.io/docs/)
3. [Maya: Date e ora per gli esseri umani](https://github.com/kennethreitz/maya)
4. [Arrow: Date e ora migliori per Python](https://arrow.readthedocs.io/en/latest/)