---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:58:02.481229-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Capire se una cartella esiste è essenziale per evitare errori nel file handling. Programmatori lo fanno per garantire che il loro codice interagisca correttamente con il filesystem.

## How to:
Python dispone di moduli integrati per verificare l'esistenza di directory. Ecco come:

```Python
import os

# Controllare se una directory esiste
def is_directory_exists(path):
    return os.path.isdir(path)

# Uso
directory_to_check = "/path/to/directory"

if is_directory_exists(directory_to_check):
    print(f"La directory {directory_to_check} esiste!")
else:
    print(f"La directory {directory_to_check} non esiste.")

# Output potrebbe essere:
# La directory /path/to/directory esiste!
# oppure
# La directory /path/to/directory non esiste.
```

O si può usare il modulo `pathlib` per un approccio più moderno:

```Python
from pathlib import Path

# Controllare se una directory esiste
def is_directory_exists(path):
    return Path(path).is_dir()

# Uso
directory_to_check = "/path/to/directory"

if is_directory_exists(directory_to_check):
    print(f"La directory {directory_to_check} esiste!")
else:
    print(f"La directory {directory_to_check} non esiste.")

# L'output sarà lo stesso dell'esempio precedente.
```

## Deep Dive
Prima della versione Python 3.4, `os.path` era l'unica scelta per lavorare con i percorsi dei file. Nel 2014, `pathlib` è stato introdotto per fornire un'interfaccia orientata agli oggetti per manipolare percorsi e file, spesso rendendo il codice più leggibile e chiaro.

Alternativamente, si può creare una directory se non esiste usando `os.makedirs` con l'opzione `exist_ok=True`, così:

```Python
os.makedirs(directory_to_check, exist_ok=True)
```

Questa riga non solleverà un errore se la directory esiste già.

Dettagli d'implementazione:
- `os.path.isdir` verifica l'esistenza della directory e se l'oggetto in questione è effettivamente una directory e non un file.
- `Path.is_dir` in `pathlib` fa lo stesso, ma opera su un oggetto `Path` che rappresenta il percorso nel filesystem.

## See Also
- Documentazione ufficiale `os` module: https://docs.python.org/3/library/os.html
- Documentazione ufficiale `pathlib` module: https://docs.python.org/3/library/pathlib.html
- Tutorial `pathlib`: https://realpython.com/python-pathlib/
- Stack Overflow: discussioni e domande comunemente fatte su File I/O in Python.
