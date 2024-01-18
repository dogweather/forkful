---
title:                "Estrarre una data da una stringa"
html_title:           "Python: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Cos'è & Perché?
Il parsing di una data da una stringa è il processo di estrarre una data significativa da una stringa di testo. Questo è molto utile per i programmatori perché consente loro di manipolare le date in modo più preciso e di svolgere operazioni come la conversione del formato della data.

## Come fare:
Di seguito sono riportati alcuni esempi di codice Python per mostrare come eseguire il parsing di una data da una stringa. 

### Esempio 1:
```Python
from datetime import datetime

my_string = "10/12/2021"
my_date = datetime.strptime(my_string, "%m/%d/%Y")
print(my_date)
```
Output: 2021-10-12 00:00:00

### Esempio 2:
```Python
import dateutil.parser as dp

my_string = "16 Settembre 2021"
my_date = dp.parse(my_string, dayfirst=True)
print(my_date)
```
Output: 2021-09-16 00:00:00

## Approfondimento:
Il processo di parsing di una data da una stringa è diventato molto importante con lo sviluppo dei computer e dei sistemi informatici. Prima di questo, le date venivano rappresentate manualmente su documenti scritti a mano o a macchina, il che rendeva difficile per i computer manipolarle. Oggi, ci sono diversi modi per eseguire il parsing di una data, come l'utilizzo di moduli come ```datetime``` e ```dateutil```. Tuttavia, è importante essere consapevoli di eventuali difetti nei formati delle date e dei fusi orari quando si effettua il parsing di una data.

## Vedi anche:
Ecco alcuni link utili per saperne di più sul parsing di una data da una stringa in Python:

- [Documentazione ufficiale di datetime in Python](https://docs.python.org/3/library/datetime.html)
- [Documentazione ufficiale di dateutil in Python](https://dateutil.readthedocs.io/en/stable/)
- [Un tutorial su come effettuare il parsing di una data in Python](https://www.geeksforgeeks.org/python-program-to-parse-a-date-of-string-to-an-object/)