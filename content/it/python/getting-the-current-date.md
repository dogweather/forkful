---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
La funzione di ottenimento della data corrente è un modo per sapere in quale giorno ci troviamo utilizzando il codice. Questo è particolarmente utile per l'assegnazione di timestamp, la creazione di cronometri o il monitoraggio dell'avanzamento dei processi.

## Come si fà:
Ecco un modo semplice per ottenere la data odierna:
``` Python 
from datetime import date

oggi = date.today()
print(oggi)
```
Esseguendo questo codice otterrete un risultato del tipo:
``` Shell
2022-04-28
```
## Approfondimento:
L'oggetto ```date``` è stato introdotto in Python 2.3 e ha subito diverse migliorie e modifiche da allora. Esistono alternative per ottenere la data attuale, come l'utilizzo della libreria ```time``` o ```pytz``` per ottenere la data corrente con il fuso orario. L'importazione di ```date``` da ```datetime``` è l'opzione più comune perché è semplice, facile da usare e fornisce funzionalità sufficienti per la maggior parte delle necessità.
 
## Vedere Anche:
Guarda la documentazione del modulo python datetime [qui](https://docs.python.org/3/library/datetime.html) per una panoramica completa delle sue funzionalità. Per informazioni più dettagliate su come lavorare con le date e gli orari in Python, consulta questo [tutorial dettagliato](https://realpython.com/python-datetime/). Paradiso dell'hacker ha anche un [ottimo articolo](https://towardsdatascience.com/working-with-datetime-in-pandas-dataframe-663f7af6c587) che esplora come lavorare con datetime in un dataframe pandas.