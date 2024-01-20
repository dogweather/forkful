---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La conversione di una data in una stringa è una componente vitale nella programmazione Python. Questa operazione, nota come "string formatting di date", consente ai programmatori di manipolare e visualizzare date nel formato desiderato, facilitando la comunicazione e l'interazione con l'utente.

## Come fare:

Vediamo alcuni esempi di come convertire una data in una stringa con Python:

```Python
from datetime import date

# Creare un oggetto data 
d = date.today()

# Convertirlo in una stringa 
data_str = d.strftime("%d/%m/%Y") 
print(data_str) 
```

Output:

```Python
'24/06/2022'
```

In questo esempio, `strftime` è la funzione che converte la nostra data in una stringa.

## Approfondimento

La conversione delle date in stringhe ha le sue radici nel linguaggio di programmazione C, da cui Python ha tratto molte delle sue funzionalità di basso livello. Gli sviluppatori Python hanno mantenuto la potenza e la flessibilità di `strftime`, fornendo nello stesso tempo una più comoda interfaccia orientata agli oggetti.

Esistono alternative a `strftime`, come `isoformat()`, che converte una data in una stringa seguendo uno standard internazionale. Detto questo, la flessibilità di `strftime` lo rende la scelta preferita nella maggior parte dei casi.

```Python
data_iso = d.isoformat()
print(data_iso)
```

Output:

```Python
'2022-06-24'
```

Un aspetto della conversione da tenere in considerazione è la gestione delle eccezioni. In alcuni casi potresti dover gestire date non valide o formattazioni non corrette. A seconda delle tue esigenze, potrebbe essere necessario utilizzare `try/except` per gestire queste situazioni.

## Vedi anche:

Se vuoi approfondire l'argomento, ecco alcuni link utili:

- [Time and Date in Python (Python Software Foundation)](https://docs.python.org/3/library/datetime.html)
- [Python strftime reference (strfti.me)](https://strftime.org/)
- [ISO 8601 Date Format (Wikipedia)](https://en.wikipedia.org/wiki/ISO_8601)