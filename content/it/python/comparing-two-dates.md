---
title:                "Python: Confrontare due date"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è un'operazione molto comune nella programmazione e può essere utile per diversi motivi. Ad esempio, potresti voler verificare se una determinata data è successiva a un'altra, o se due date corrispondono alla stessa giornata. In questo articolo, impariamo come effettuare confronti tra date utilizzando il linguaggio di programmazione Python.

## Come fare

Per confrontare due date in Python, utilizziamo il modulo `datetime` che ci permette di lavorare con oggetti di tipo data e ora. Per prima cosa, importiamo il modulo nel nostro script:

```Python
import datetime
```

Ora, possiamo creare due oggetti date utilizzando la classe `date` del modulo `datetime` e specificando l'anno, il mese e il giorno desiderati:

```Python
date1 = datetime.date(2019, 7, 15)
date2 = datetime.date(2020, 1, 1)
```

Per verificare se `date1` è successiva a `date2`, possiamo utilizzare l'operatore `>`:

```Python
if date1 > date2:
    print("date1 è successiva a date2")
```

Questa condizione sarà vera, poiché 15 luglio 2019 viene prima del 1 gennaio 2020. Possiamo anche confrontare due date ugualmente specificate per verificare se corrispondono alla stessa giornata:

```Python
if date1 == date2:
    print("Le due date corrispondono")
else:
    print("Le due date non corrispondono")
```

In questo caso, la seconda condizione sarà eseguita poiché le due date sono diverse.

## Approfondimento

Oltre ai confronti semplici, il modulo `datetime` ci offre anche altre funzionalità utili per la gestione delle date. Ad esempio, possiamo calcolare la differenza tra due date e ottenere il numero di giorni o mesi che le separano utilizzando la classe `timedelta`:

```Python
delta = date2 - date1
print(delta.days) # output: 170
print(delta.months) # errore: timedelta object has no attribute 'months'
```

Come possiamo vedere, il risultato del calcolo è un oggetto `timedelta` che ci permette di accedere ai giorni (`delta.days`) ma non ai mesi.

Inoltre, possiamo facilmente convertire una data in una stringa utilizzando il metodo `strftime` e specificando il formato desiderato:

```Python
date_string = date1.strftime("%d/%m/%Y") # output: 15/07/2019
```

Per conoscere tutti i possibili formati delle date, consultare la documentazione ufficiale di Python sul modulo `datetime`.

## Vedi anche

- [Documentazione ufficiale di Python su `datetime`](https://docs.python.org/3/library/datetime.html)
- [Come utilizzare il modulo `datetime` in Python](https://www.programiz.com/python-programming/datetime)