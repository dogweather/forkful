---
title:    "Python: Calcolare una data nel futuro o nel passato"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché 
Il calcolo di una data nel futuro o nel passato può essere utile in molte situazioni, ad esempio per pianificare eventi, tenere traccia dei giorni lavorativi o semplicemente per soddisfare la curiosità.

## Come
La libreria `datetime` di Python offre delle funzioni utili per il calcolo delle date. Per calcolare una data nel futuro, si può utilizzare la funzione `date.today()` per ottenere la data odierna e successivamente la funzione `timedelta(days=x)` per aggiungere un numero di giorni desiderato. Ad esempio:

```Python
import datetime

oggi = datetime.date.today()
domani = oggi + datetime.timedelta(days=1)

print(domani)
# output: 2021-03-05
```

Per calcolare una data nel passato, si può utilizzare la funzione `timedelta(days=-x)` per sottrarre un numero di giorni dalla data odierna. Ad esempio:

```Python
ieri = oggi - datetime.timedelta(days=1)

print(ieri)
# output: 2021-03-03
```

## Approfondimento
Oltre alle funzioni `date.today()` e `timedelta()`, la libreria `datetime` offre anche altre utili funzioni come `date.replace()` per modificare una data esistente, `date.weekday()` per ottenere il giorno della settimana corrispondente e `date.strftime()` per formattare la data in modi diversi. 

Un altro aspetto importante da considerare è il formato della data. In Python, le date vengono di default rappresentate nel formato `yyyy-mm-dd`, ma è possibile utilizzare la funzione `strptime()` per convertire una data in un formato personalizzato o utilizzare la libreria `locale` per ottenere il formato della data specifico per la lingua e la regione.

## Vedi anche
- [Documentazione ufficiale della libreria `datetime`](https://docs.python.org/3/library/datetime.html)
- [Tutorial su come lavorare con le date in Python](https://realpython.com/python-datetime/)
- [Esempi pratici di utilizzo della libreria `datetime`](https://www.programiz.com/python-programming/datetime)