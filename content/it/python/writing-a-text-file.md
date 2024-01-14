---
title:                "Python: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è un'operazione essenziale nella programmazione Python. Può essere utile per salvare dati, generare report o creare documentazione per il proprio codice.

## Come fare

Per scrivere un file di testo in Python, iniziamo importando il modulo "os", che ci permetterà di accedere al sistema operativo. Utilizziamo il comando "open" per creare un nuovo file di testo e scegliere la modalità di scrittura (*w* per scrittura, *a* per aggiunta). Assicuriamoci di specificare il percorso completo del file, altrimenti verrà creato nella directory di lavoro corrente.

```Python
import os

file = open("path/file.txt", "w")

# scrittura di dati nel file
file.write("Questo è un esempio di testo scritto in un file di testo con Python.")

file.close()
```

Dopo aver scritto i dati nel file, è importante chiuderlo utilizzando il comando "close()". In caso contrario, le modifiche apportate al file potrebbero non essere salvate.

## Approfondimento

Esistono diverse modalità di scrittura di un file di testo in Python, come "w+" che permette di aprire un file per la scrittura e la lettura o "x" che crea un nuovo file ma fallisce se il file esiste già. Inoltre, possiamo utilizzare il comando "with" per gestire automaticamente la chiusura del file dopo averlo utilizzato.

```Python
# scrittura e lettura del file
with open("path/file.txt", "w+") as file:
    file.write("Questo è un esempio di testo scritto in un file di testo con Python.")

    # torna all'inizio del file
    file.seek(0)

    # legge il contenuto del file
    content = file.read()
    print(content)
```

Inoltre, possiamo utilizzare il modulo "csv" per scrivere dati in un file CSV o il modulo "json" per scrivere dati in un file JSON.

## Vedi anche

* [Documentazione su come scrivere file di testo in Python](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
* [Tutorial su come manipolare file di testo in Python](https://realpython.com/working-with-files-in-python/)