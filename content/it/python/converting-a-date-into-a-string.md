---
title:                "Python: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con date in un programma Python, si potrebbe avere la necessità di convertire una data in una stringa. Questo può essere utile per mostrare la data in un formato specifico o per salvare la data in un file di testo.

## Come fare

Per convertire una data in una stringa in Python, possiamo utilizzare il metodo `strftime()` della libreria `datetime`. Questo metodo ci permette di specificare il formato desiderato per la data e di ottenere una stringa con la data convertita.

Ecco un esempio di codice:

```Python
import datetime

# Definiamo una data
data = datetime.datetime(2021, 10, 20)

# Utilizziamo il metodo strftime() per convertire la data in una stringa nel formato "giorno/mese/anno"
stringa_data = data.strftime("%d/%m/%Y")

# Stampiamo la stringa risultante
print(stringa_data)
```

L'output di questo codice sarà `20/10/2021`.

Ci sono molti formati diversi che possiamo utilizzare con il metodo `strftime()`, come per esempio `%Y-%m-%d` per ottenere una stringa nel formato "anno-mese-giorno". È possibile trovare una lista completa dei formati disponibili nella documentazione ufficiale di Python.

## Approfondimento

È interessante notare che il metodo `strftime()` non solo ci permette di convertire una data in una stringa, ma possiamo anche utilizzarlo per convertire una stringa in una data. Basta utilizzare il metodo `strptime()` invece di `strftime()`.

Ecco un esempio di codice per convertire una stringa in una data:

```Python
import datetime

# Definiamo una stringa che rappresenta una data
stringa_data = "25/12/2021"

# Utilizziamo il metodo strptime() per convertire la stringa in una data
data = datetime.datetime.strptime(stringa_data, "%d/%m/%Y")

# Stampiamo la data risultante
print(data)
```

L'output sarà `2021-12-25 00:00:00`, che è una data valida in formato datetime di Python.

Inoltre, è importante notare che il metodo `strftime()` e `strptime()` richiedono di specificare il formato della data nel quale la stringa è scritta. Se non rispettiamo il formato desiderato, otterremo un errore.

## Vedi anche

- Documentazione ufficiale di Python per il metodo `strftime()`: https://docs.python.org/3/library/datetime.html#datetime.datetime.strftime
- Lista dei formati disponibili per il metodo `strftime()`: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes