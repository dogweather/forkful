---
title:                "Lavorare con i file csv"
html_title:           "Python: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Lavorare con i file CSV, o "Comma Separated Values", significa manipolare dei dati che sono organizzati in modo tabellare, con ciascun dato separato da una virgola. I programmatori spesso lavorano con CSV poiché è uno dei formati di file più comuni per scambiare dati tra diverse applicazioni.

## Come fare:

```python
import csv

# Aprire un file CSV in modalità lettura 
with open('data.csv', 'r') as file:
    # Creare un oggetto lettore CSV 
    reader = csv.reader(file)
    
    # Ciclare attraverso ogni riga del file 
    for row in reader:
        # Stampare ogni colonna separata da una virgola 
        print(', '.join(row))
      
# Creare un nuovo file CSV in modalità scrittura 
with open('new_data.csv', 'w') as file:
    # Creare un oggetto scrittore CSV 
    writer = csv.writer(file)
    
    # Scrivere una riga nel file 
    writer.writerow(['Nome', 'Cognome', 'Età'])
    
    # Scrivere altre righe con una lista di valori 
    writer.writerows([
        ['Mario', 'Rossi', 30],
        ['Giulia', 'Bianchi', 25]
    ])
```

Output:
```
Colonna1, Colonna2, Colonna3
Dato1, Dato2, Dato3
```

## Approfondimento:

I file CSV sono stati introdotti per la prima volta negli anni '70 come una semplice soluzione per scambiare dati tra fogli elettronici. Nel corso degli anni, sono diventati uno standard nella comunicazione dei dati tra diverse applicazioni e piattaforme. Esistono anche altri formati di file per i dati tabellari, come il formato Excel XLSX, ma il CSV rimane uno dei più usati.

## Vedi anche:

- [Documentazione ufficiale di Python per il modulo CSV](https://docs.python.org/3/library/csv.html)
- [Guida introduttiva al lavoro con CSV in Python](https://www.programiz.com/python-programming/csv)