---
title:                "Python: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché controllare se una directory esiste in Python?

Quando si lavora con file e directory in Python, può essere utile sapere se una determinata directory esiste o meno. Ciò può essere utile per gestire le operazioni di lettura e scrittura dei file, per evitare errori durante l'elaborazione dei dati. In questo articolo, vedremo come controllare se una directory esiste in Python e come gestire questa situazione.

## Come fare il controllo

Per controllare se una directory esiste in Python, utilizziamo il modulo `os` e il suo metodo `path.exists()`. Questo metodo prende in input il percorso della directory e restituisce un valore booleano che indica se la directory esiste o meno.

```Python
import os

directory = "./my_dir"

if os.path.exists(directory):  # controlla se la directory esiste
    print(f"La directory {directory} esiste.")
else:
    print(f"La directory {directory} non esiste.")
```

Output:
```shell
La directory ./my_dir esiste.
```

Se la directory esiste, il codice stamperà un messaggio che lo conferma. In caso contrario, verrà stampato un altro messaggio. Nota che il percorso della directory può essere sia assoluto che relativo rispetto al file Python in cui viene eseguito il codice.

## Approfondimento

Oltre al metodo `path.exists()`, il modulo `os` offre anche altri metodi utili per manipolare directory e file in Python. Ad esempio, `os.makedirs()` crea una nuova directory, inclusi tutti i sottodirectory necessari se non esistono già. `os.listdir()` restituisce una lista con i nomi dei file e delle directory contenuti nella directory specificata. Mentre `os.path.isfile()` e `os.path.isdir()` controllano se un determinato percorso corrisponde a un file o a una directory esistente.

## Vedi anche

- Documentazione ufficiale di Python su `os.path`: https://docs.python.org/3/library/os.path.html
- Tutorial su gestione di file e directory in Python: https://realpython.com/working-with-files-in-python/
- Esempi di codice sorgente: https://github.com/python/cpython/tree/master/Lib/os.py