---
title:                "Confrontare due date"
html_title:           "Python: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

 Spesso nelle applicazioni, è necessario confrontare due date per eseguire operazioni come il calcolo della differenza tra esse o il controllo di quali siano più recenti o più vecchie. La comparazione delle date è un'operazione comune nella programmazione e può essere utile in molti contesti diversi. 

## Come Fare

Per confrontare due date in Python, è possibile utilizzare il modulo `datetime` e la sua classe `date`. Di seguito è riportato un esempio di codice che mostra come confrontare due date:

```Python
from datetime import date

# Creazione delle due date da confrontare
first_date = date(2020, 7, 11)
second_date = date(2021, 1, 1)

# Confronto delle date
if first_date < second_date:
    print("La prima data è precedente alla seconda data")

if first_date > second_date:
    print("La prima data è successiva alla seconda data")

if first_date == second_date:
    print("Le due date sono uguali")
```

L'output del codice sarà:

```
La prima data è precedente alla seconda data
```

Per confrontare date con orari specifici, si può utilizzare invece la classe `datetime`, sempre all'interno del modulo `datetime`.

## Approfondimento

In Python, le date sono rappresentate come oggetti, con proprietà e metodi associati. In particolare, l'oggetto `date` ha alcune proprietà interessanti che possono essere utili nel confronto tra due date:

- `year`: restituisce l'anno della data
- `month`: restituisce il mese della data
- `day`: restituisce il giorno della data
- `weekday()`: restituisce il giorno della settimana in formato numerico (0 è lunedì, 6 è domenica)
- `isoweekday()`: restituisce il giorno della settimana in formato numerico (1 è lunedì, 7 è domenica)

Ad esempio, è possibile utilizzare la proprietà `weekday()` per confrontare due date in base al giorno della settimana:

```Python
from datetime import date

# Creazione delle due date da confrontare
first_date = date(2021, 7, 11) # domenica
second_date = date(2021, 7, 12) # lunedì

# Confronto delle date
if first_date.weekday() < second_date.weekday():
    print("La prima data è prima della seconda data rispetto al giorno della settimana")

if first_date.weekday() > second_date.weekday():
    print("La prima data è dopo della seconda data rispetto al giorno della settimana")

if first_date.weekday() == second_date.weekday():
    print("Le due date sono uguali rispetto al giorno della settimana")
```

L'output del codice sarà:

```
La prima data è prima della seconda data rispetto al giorno della settimana
```

## Vedi Anche

- Documentazione ufficiale del modulo `datetime`: https://docs.python.org/3/library/datetime.html
- Tutorial su come lavorare con le date in Python: https://realpython.com/python-datetime/