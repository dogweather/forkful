---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"

category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con i CSV significa manipolare dati in un formato testuale semplice, molto usato per l'esportazione e l'importazione di informazioni tra programmi e sistemi. I programmatori lo fanno per la sua universalità e la facilità nel trasferire dati tabellari.

## How to:
```Python
import csv

# Lettura di un file CSV
with open('esempio.csv', mode='r', newline='') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)

# Scrittura in un file CSV
with open('esempio.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['nome', 'cognome', 'età'])
    writer.writerow(['Giulia', 'Rossi', 30])
    writer.writerow(['Marco', 'Verdi', 45])

# Output di lettura
['nome', 'cognome', 'età']
['Giulia', 'Rossi', '30']
['Marco', 'Verdi', '45']
```

## Deep Dive
I CSV sono così vecchi quanto i primi computer. Alternativamente, si possono usare JSON o XML, più strutturati e con maggiore supporto per tipi di dati complessi, ma i CSV rimangono la scelta principale quando si maneggiano dati tabellari semplici per la loro leggibilità umana e compatibilità con fogli di calcolo. In Python, il modulo CSV gestisce dettagli come le virgolette e i problemi di codifica dei file, semplificando quest'operazioni.

## See Also
- Python CSV documentation: https://docs.python.org/3/library/csv.html
- Tutorial CSV con Python: https://realpython.com/python-csv/
- Esplorare dati con Pandas: https://pandas.pydata.org/
