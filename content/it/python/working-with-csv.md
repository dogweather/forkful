---
title:                "Python: Lavorare con csv"
simple_title:         "Lavorare con csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché lavorare con CSV in Python

Quando si lavora con grandi quantità di dati, è fondamentale avere un formato standard e compatibile per la loro memorizzazione e condivisione. I file CSV (Comma-Separated Values) sono un formato molto utilizzato in ambito informatico e possono essere letti e modificati da diversi programmi e linguaggi di programmazione, inclusa Python. In questo articolo vedremo come utilizzare Python per lavorare con i file CSV in modo efficiente.

## Come lavorare con CSV in Python

Per prima cosa, è necessario importare il modulo `csv` in Python. Si può fare con il seguente codice:

```Python
import csv
```

Per aprire un file CSV, si utilizza la funzione `open()` specificando il nome del file e il tipo di operazione (lettura, scrittura, etc.). Ad esempio, per aprire un file chiamato "dati.csv" per la lettura, si utilizza il seguente codice:

```Python
with open("dati.csv", "r") as file:
    # codice per la lettura e la manipolazione dei dati
```

Una volta aperto il file, è possibile leggerne il contenuto utilizzando un `csv.reader` e iterando sulle righe del file. Ad esempio, per stampare tutto il contenuto del file, si può utilizzare il seguente codice:

```Python
with open("dati.csv", "r") as file:
    dati_csv = csv.reader(file)
    for riga in dati_csv:
        print(riga)
```

Se le righe del file sono delimitate da un carattere diverso dalla virgola, è possibile specificarlo come argomento della funzione `csv.reader`. Per esempio, per un file con righe delimitate dal punto e virgola, si può utilizzare il codice seguente:

```Python
with open("dati.csv", "r") as file:
    dati_csv = csv.reader(file, delimiter=";")
    for riga in dati_csv:
        print(riga)
```

Per scrivere dei dati in un file CSV, si utilizza invece la funzione `csv.writer`. Ad esempio, per scrivere una nuova riga nel file "dati.csv", si utilizza il seguente codice:

```Python
with open("dati.csv", "a", newline="") as file:
    dati_csv = csv.writer(file)
    nuova_riga = ["John", "Doe", "35"]
    dati_csv.writerow(nuova_riga)
```

## Approfondimento sul lavoro con CSV

Il modulo `csv` di Python offre molte altre funzionalità utili per lavorare con file CSV. Ad esempio, è possibile utilizzare la funzione `DictReader` per leggere il contenuto di un CSV come un dizionario, con le chiavi corrispondenti ai nomi delle colonne. Inoltre, il modulo `csv` supporta anche la lettura e la scrittura di file CSV in diversi formati, come ad esempio il formato Microsoft Excel.

È importante anche tenere in considerazione alcune considerazioni sulla gestione dei dati nei file CSV, come ad esempio la presenza di virgolette nei dati e la gestione dei valori mancanti. Per ulteriori informazioni e approfondimenti, si consiglia di consultare la documentazione ufficiale del modulo `csv` di Python.

# Vedi anche

- Documentazione ufficiale del modulo `csv` di Python: https://docs.python.org/3/library/csv.html
- Tutorial su come lavorare con file CSV in Python: https://realpython.com/python-csv/
- Esempi pratici di utilizzo dei file CSV in Python: https://www.youtube.com/watch?v=q5uM4VKywbA