---
title:                "Ottenere la data attuale"
html_title:           "Python: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Capita spesso che abbiamo bisogno di utilizzare la data o l'ora attuale in un progetto Python. Può essere utile per una varietà di applicazioni, come registri giornalieri, generazione di nomi file dinamici, o semplicemente per tenere traccia delle informazioni temporali nei nostri programmi.

## Come

Per ottenere la data e l'ora attuali in Python, possiamo utilizzare il modulo `datetime`. Possiamo importarlo nel nostro script con `import datetime` e poi utilizzare i suoi metodi per ottenere la data attuale. Ad esempio:

```Python
import datetime

oggi = datetime.date.today()
print(oggi)

ora_attuale = datetime.datetime.now()
print(ora_attuale)
```

Questo produrrà un'output simile al seguente:

```
2021-10-08
2021-10-08 12:30:00.043902
```

Possiamo personalizzare anche il formato della data o dell'ora utilizzando il metodo `strftime` (abbreviazione di "string format time"). Ad esempio:

```Python
import datetime

oggi = datetime.date.today()
print(oggi.strftime("%A, %B %d, %Y"))

ora_attuale = datetime.datetime.now()
print(ora_attuale.strftime("%H:%M:%S"))
```

Questo produrrà un'output simile al seguente:

```
Venerdì, Ottobre 08, 2021
12:30:00
```

## Deep Dive

Il modulo `datetime` ci offre anche altri metodi per manipolare le date e le ore. Ad esempio, possiamo creare un oggetto `datetime` personalizzato utilizzando il metodo `datetime()` e specificando una data e un'ora specifiche. Possiamo anche eseguire operazioni matematiche tra oggetti datetime, come calcolare la differenza tra due date o aggiungere un intervallo di tempo specificato ad una data. 

Inoltre, è importante tenere in considerazione il fuso orario quando si lavora con la data e l'ora attuali, poiché possono variare da un sistema all'altro. Possiamo utilizzare il modulo `pytz` per gestire i fusi orari in modo preciso.

## Vedi anche (See Also)

- Documentazione ufficiale del modulo datetime: https://docs.python.org/3/library/datetime.html
- Domande frequenti su datetime in Python: https://docs.python.org/3/library/datetime.html#faq
- Modulo pytz: https://pypi.org/project/pytz/