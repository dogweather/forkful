---
title:                "Verifica se una directory esiste"
html_title:           "PHP: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Controllo dell'esistenza di una directory in Python

## Cos'è e Perché?
Il controllo dell'esistenza di una directory è un'operazione per verificare se una specifica cartella esiste sul tuo sistema di file. Questo è essenziale per evitare errori durante l'esecuzione di script che operano con i file.

## Come fare:
In Python, possiamo utilizzare il modulo `os` e il metodo `os.path` per verificare se una directory esiste.

```Python
import os

if os.path.isdir('/path/to/directory'):
    print("La directory esiste.")
else:
    print("La directory non esiste.")
```
Se la directory esiste, il tuo output sarà:
```
La directory esiste.
```
Se la directory non esiste, il tuo output sarà:
```
La directory non esiste.
```

## Approfondimento
L'uso del modulo `os` in Python risale alle prime versioni del linguaggio. Nel corso del tempo, sono state sviluppate diverse alternative, ma l'utilizzo di `os.path.isdir()` rimane il modo più comune e semplice per verificare l'esistenza di una directory.

Un'alternativa potrebbe essere l'uso del modulo `pathlib`, introdotto in Python 3.4. Il `pathlib` offre un approccio ad orientamento oggettuale alla gestione dei percorsi. Ad esempio:

```Python
from pathlib import Path

if Path('/path/to/directory').exists():
    print("La directory esiste.")
else:
    print("La directory non esiste.")
```

L'implementazione dietro l'operazione di controllo dell'esistenza di una directory si basa su funzioni di sistema a basso livello, che variano a seconda del sistema operativo. In generale, Python si interfaccia con queste funzioni di sistema e fornisce un'interfaccia di alto livello per utilizzarle.

## Vedi anche
- Documentazione ufficiale del modulo [`os`](https://docs.python.org/3/library/os.html)
- Documentazione ufficiale del modulo [`pathlib`](https://docs.python.org/3/library/pathlib.html)
- Guida di Python su [come leggere e scrivere file](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- Python per Principianti: [Operazioni di Base sui File](https://tutorial.djangogirls.org/it/python_introduction/#operazioni-di-base-sui-file)