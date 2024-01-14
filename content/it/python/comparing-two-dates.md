---
title:    "Python: Confrontare due date"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

In programmazione, spesso ci troviamo nella situazione in cui dobbiamo confrontare due date. Ad esempio, potremmo voler sapere se un evento è avvenuto prima o dopo un altro evento, o se una data rientra in un determinato intervallo di tempo. Confrontare le date è un'operazione comune e importante nella gestione dei dati e nella creazione di applicazioni. In questo articolo, esploreremo come comparare due date utilizzando il linguaggio di programmazione Python.

## Come Fare

Per confrontare due date in Python, dobbiamo innanzitutto comprendere che le date sono rappresentate come oggetti nella libreria standard del linguaggio. In particolare, useremo il modulo `datetime` che ci permette di creare oggetti data utilizzando il costruttore `datetime.date(year, month, day)`.

Iniziamo importando il modulo `datetime`:

```Python
import datetime
```

Creiamo quindi due oggetti data, rappresentando il 1 gennaio 2021 e il 1 marzo 2021:

```Python
date_1 = datetime.date(2021, 1, 1)
date_2 = datetime.date(2021, 3, 1)
```

Per confrontare le date, possiamo utilizzare gli operatori di confronto come `>`, `<`, `==` o `!=`. Se vogliamo verificare se una data è successiva a un'altra, usiamo l'operatore maggiore `>`:

```Python
date_1 > date_2 # output: False
```

Al contrario, se vogliamo verificare se una data è precedente a un'altra, usiamo l'operatore minore `<`:

```Python
date_1 < date_2 # output: True
```

Possiamo anche verificare l'uguaglianza tra due date utilizzando l'operatore `==`:

```Python
date_1 == date_2 # output: False
```

E infine, possiamo verificare se le due date sono diverse utilizzando l'operatore `!=`:

```Python
date_1 != date_2 # output: True
```

Notiamo che questi confronti sono possibili anche tra oggetti di tipo `datetime.datetime`, che rappresentano una data e un'ora specifica.

## Approfondimento

In Python, le date vengono gestite come oggetti immutabili, cioè non possono essere modificati dopo la loro creazione. Inoltre, le date sono rappresentate come interi, il che significa che possiamo utilizzare anche operazioni matematiche per confrontarle.

Ad esempio, possiamo calcolare la differenza in giorni tra due date utilizzando l'operatore sottrazione `-`.

```Python
diff_days = date_2 - date_1 # output: 59 days, 0:00:00
```

Possiamo anche aggiungere o sottrarre un certo numero di giorni a una data utilizzando l'operatore somma `+` o sottrazione `-`.

```Python
new_date = date_1 + datetime.timedelta(days=7) # output: 2021-01-08
```

Infine, se vogliamo visualizzare una data in un formato specifico, possiamo utilizzare il metodo `strftime()` che ci permette di formattare una data in una stringa nel formato desiderato.

```Python
formatted_date = date_1.strftime("%d/%m/%Y") # output: 01/01/2021
```

In questo breve articolo abbiamo esplorato come comparare due date in Python utilizzando gli operatori di confronto e altre operazioni. Speriamo che queste informazioni ti siano utili nella tua programmazione quotidiana.

## Vedi Anche

- [Documentazione ufficiale di Python sul modulo datetime](https://docs.python.org/3/library/datetime.html)
- [Tutorial su come utilizzare il modulo datetime in Python](https://realpython.com/python-datetime/)
- [Esempi di confronto tra date in Python](https://www.programiz.com/python-programming/datetime/compare-dates)