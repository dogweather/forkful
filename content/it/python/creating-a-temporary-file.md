---
title:                "Creazione di un file temporaneo"
html_title:           "Python: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Puoi creare un file temporaneo per salvare temporaneamente dei dati durante l'esecuzione di un programma o per evitare di sovraccaricare eccessivamente la memoria.

## Come fare

L'utilizzo di file temporanei in Python è abbastanza semplice. Possiamo utilizzare il modulo `tempfile` per crearne uno e manipolarlo secondo le nostre esigenze. Alcune delle funzioni più utilizzate sono:

- `tempfile.TemporaryFile()`: crea un file temporaneo scrivibile e leggibile;
- `tempfile.NamedTemporaryFile()`: crea un file temporaneo con un nome specificato;
- `tempfile.SpooledTemporaryFile()`: crea un file temporaneo che viene salvato in memoria fino a quando non viene superata una dimensione massima, dopo di che viene salvato su disco;
- `tempfile.mkstemp()`: crea un file temporaneo e restituisce un descrittore e il percorso del file.

Ecco un esempio di codice che mostra come utilizzare `TemporaryFile()` per scrivere e leggere dei dati:

```Python
import tempfile

# Creazione di un file temporaneo
with tempfile.TemporaryFile() as tmp:
    # Scrivo dei dati nel file
    tmp.write(b"Questo è un esempio di file temporaneo.")

    # Ritorno all'inizio del file
    tmp.seek(0)

    # Leggo i dati dal file
    data = tmp.read()
    print(data)
```

E questo è il risultato:

```Python
b"Questo è un esempio di file temporaneo."
```

## Un approfondimento

Creare un file temporaneo può essere utile per migliorare le prestazioni del tuo programma. Infatti, invece di dover gestire grandi quantità di dati in memoria, è possibile salvarli su un file temporaneo e liberare la memoria per altre operazioni.

Inoltre, questo approccio è molto utile quando si lavora con dati sensibili, come informazioni personali o password. Una volta che il file temporaneo viene chiuso, viene automaticamente eliminato dal disco, evitando così possibili rischi di sicurezza.

## Vedi anche

- Documentazione ufficiale di Python sulla gestione dei file temporanei: https://docs.python.org/3/library/tempfile.html
- Un tutorial su come utilizzare i file temporanei in Python: https://realpython.com/python-tempfile/