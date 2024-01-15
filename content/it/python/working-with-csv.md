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

## Perché

Se sei un programmatore Python, probabilmente hai sentito parlare dei file CSV. Questi sono semplici file di testo contenenti dati separati da virgole (o altri separatori). Ma perché dovresti lavorare con questi file? Beh, i CSV sono comunemente utilizzati per scrivere dati tabellari come fogli di calcolo o database, quindi capire come lavorare con loro può essere molto utile per una varietà di scopi.

## Come fare

Per prima cosa, dovrai importare il modulo CSV nella tua applicazione Python:

```Python
import.csv
```

Per aprire un file CSV esistente, puoi utilizzare la funzione "open" con il parametro "r" (lettura). Ad esempio, se il tuo file CSV si chiama "esempio.csv", il codice sarà il seguente:

```Python
with open('esempio.csv', 'r') as file:
  reader = csv.reader(file)
  for row in reader:
    print(row)
```
Questo codice leggerà il file CSV e quindi ne stamperà ogni riga come una lista di valori separati da virgole.

Se vuoi creare un nuovo file CSV, puoi utilizzare la funzione "writer" e specificare il parametro "w" (scrittura). Ad esempio, per creare un nuovo file "output.csv" con alcune righe di dati, puoi usare il seguente codice:

```Python
with open('output.csv', 'w') as file:
  writer = csv.writer(file)
  writer.writerow(['Nome', 'Età', 'Email'])
  writer.writerow(['Giulia', '25', 'giulia@email.com'])
  writer.writerow(['Andrea', '32', 'andrea@email.com'])
```

Questo codice creerà un nuovo file CSV con il nome, l'età e l'email di due persone come righe.

## Approfondimento

Ora che hai visto alcuni esempi di base su come lavorare con CSV in Python, ecco alcune informazioni più approfondite che possono esserti utili:

- Puoi specificare un altro separatore di campi usando il parametro "delimiter" all'interno delle funzioni "reader" e "writer". Ad esempio, se il tuo CSV utilizza il punto e virgola come separatore invece della virgola, puoi impostarlo in questo modo: "csv.reader(file, delimiter=';')"

- Per scrivere e leggere intere strutture di dati come dizionari invece di liste, puoi utilizzare le funzioni "DictReader" e "DictWriter" anziché "reader" e "writer".

- Se incontri errori come "UnicodeDecodeError" quando si lavora con CSV, puoi provare ad utilizzare il parametro "encoding" per specificare il formato dei caratteri del file CSV, ad esempio "utf-8" o "latin-1".

## Vedi anche

- Documentazione Python CSV: https://docs.python.org/3/library/csv.html
- Tutorial su CSV in Python: https://realpython.com/python-csv/
- Esempi pratici di CSV: https://www.programiz.com/python-programming/examples/csv