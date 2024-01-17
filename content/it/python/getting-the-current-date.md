---
title:                "Ottenere la data corrente"
html_title:           "Python: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La data corrente è semplicemente la data odierna. Spesso i programmatori hanno bisogno di ottenere la data corrente per registrare l'orario di un evento o per gestire scadenze. 

## Come fare:
Per ottenere la data corrente in Python, è possibile utilizzare il modulo datetime integrato. Ecco un esempio di codice che mostra la data corrente nel formato desiderato:
```Python
import datetime
# Ottieni la data e l'ora corrente
current_date = datetime.datetime.now()
# Stampa la data nel formato dd/mm/yyyy
print(current_date.strftime('%d/%m/%Y'))
```
Output:
```
15/04/2021
```

## Approfondimento:
Il modulo datetime è stato introdotto nella versione 2.3 di Python ed è considerato uno dei moduli più utili per la gestione del tempo. Tuttavia, esistono anche altri moduli e librerie come time e calendar che possono essere utilizzati per ottenere la data corrente in diversi formati.

## Vedi anche:
- [Documentazione ufficiale di datetime](https://docs.python.org/3/library/datetime.html)
- [Tutorial su come utilizzare il modulo datetime in Python](https://www.programiz.com/python-programming/datetime)
- [Moduli integrati per la gestione del tempo in Python](https://www.geeksforgeeks.org/python-modules-for-time-and-date-manipulation/)