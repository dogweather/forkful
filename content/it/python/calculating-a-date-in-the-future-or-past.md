---
title:                "Calcolare una data nel futuro o passato."
html_title:           "Python: Calcolare una data nel futuro o passato."
simple_title:         "Calcolare una data nel futuro o passato."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Il calcolo di una data nel futuro o nel passato può essere utile per programmatori che lavorano su progetti che richiedono la gestione di date o per chiunque voglia conoscere la data esatta in cui un evento accadrà o è accaduto. 

## Come

Per calcolare una data nel futuro o nel passato, possiamo utilizzare il modulo `datetime` di Python, che offre diverse funzioni utili per la gestione delle date. Ecco un esempio di codice che calcola la data esatta tra 10 giorni:

```Python
import datetime

oggi = datetime.date.today()
futura_data = oggi + datetime.timedelta(days=10)
print(futura_data)
```

Questo codice utilizza la funzione `timedelta()` per aggiungere 10 giorni alla data attuale e poi stampa la data risultante. Possiamo anche utilizzare altre unità di tempo come `weeks`, `hours`, `minutes` ecc. per calcolare una data futura o passata.

È inoltre possibile specificare una data specifica utilizzando la sintassi seguente: `date(year, month, day)`. Ad esempio, per calcolare la data esatta tra 1 anno, 2 mesi e 3 giorni, possiamo utilizzare il seguente codice:

```Python
futura_data = datetime.date(2022, 4, 3)
print(futura_data)
```

Questo codice stamperà la data "2022-04-03".

## Deep Dive

Il modulo `datetime` offre anche diverse altre funzioni utili per la gestione delle date. Ad esempio, possiamo utilizzare la funzione `strftime()` per formattare una data in un formato specifico. Ecco un esempio di codice che stampa la data attuale nel formato "dd/mm/yyyy":

```Python
data_attuale = datetime.date.today()
print(data_attuale.strftime("%d/%m/%Y"))
```

Il simbolo `%d` rappresenta il giorno, `%m` il mese e `%Y` l'anno. Possiamo combinare questi simboli in qualsiasi ordine per ottenere il formato desiderato. 

Inoltre, è possibile utilizzare il modulo `calendar` per ottenere informazioni su specifici mesi o anni. Ad esempio, il seguente codice stamperà il numero di giorni in un determinato mese e anno:

```Python
import calendar

mese = 9 # settembre
anno = 2021
print(calendar.monthrange(anno, mese)[1])
```

Il metodo `monthrange()` restituisce una tupla contenente il giorno della settimana in cui inizia il mese e il numero di giorni in quel mese. Nel nostro esempio, otteniamo 30, il numero di giorni in settembre 2021.

## Vedi anche

- [Documentazione ufficiale di Python sul modulo datetime](https://docs.python.org/3/library/datetime.html)
- [Tutorial su come utilizzare il modulo datetime di Python](https://realpython.com/python-datetime/)