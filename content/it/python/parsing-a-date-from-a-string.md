---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

---
## Cosa & Perché?

L'analisi di una data da una stringa significa trasformare una stringa che rappresenta una data in un oggetto di tipo datetime in Python. Lo facciamo per poter manipolare e confrontare facilmente le date nel nostro codice.

## Come fare:

Parsing di una data da una stringa con il modulo datetime di Python:
```Python
from datetime import datetime
data_stringa = "12/05/2021"
data = datetime.strptime(data_stringa, "%d/%m/%Y")
print(data)
```
Risultato:
```Python
2021-05-12 00:00:00
```

## Approfondimenti:

(1) Contesto storico - L'elaborazione di una data da una stringa è stato un problema comune sin dall'inizio della programmazione, con vari metodi sviluppati per differenti linguaggi di programmazione.

(2) Alternative - Oltre a `strptime`, Python offre anche `dateutil.parser.parse`, un metodo più flessibile capace di interpretare formati di data più vari:
```Python
from dateutil.parser import parse
data_stringa = "12/05/2021"
data = parse(data_stringa)
print(data)
```

(3) Dettagli implementativi - Entrambi i metodi sopra trasformano una stringa in un oggetto datetime. Questa trasformazione viene effettuata interpretando la stringa secondo un formato dato (che indica la posizione di giorno, mese e anno) e creando l'oggetto di data corrispondente.

## Per approfondire:

- Documentazione ufficiale Python su datetime: https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior
- Libreria dateutil: https://dateutil.readthedocs.io/en/stable/
- Guida Python su stringhe e datetime: https://www.w3schools.com/python/python_datetime.asp