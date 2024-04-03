---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:12.741764-07:00
description: "Come fare: Python fornisce modi nativi per controllare l'esistenza di\
  \ una directory utilizzando i moduli `os` e `pathlib`. Ecco degli esempi per entrambi:\u2026"
lastmod: '2024-03-13T22:44:43.015569-06:00'
model: gpt-4-0125-preview
summary: Python fornisce modi nativi per controllare l'esistenza di una directory
  utilizzando i moduli `os` e `pathlib`.
title: Verifica se una directory esiste
weight: 20
---

## Come fare:
Python fornisce modi nativi per controllare l'esistenza di una directory utilizzando i moduli `os` e `pathlib`. Ecco degli esempi per entrambi:

### Utilizzando il modulo `os`
```python
import os

# Specificare il percorso della directory
dir_path = "/percorso/alla/directory"

# Controllare se la directory esiste
if os.path.isdir(dir_path):
    print(f"La directory {dir_path} esiste.")
else:
    print(f"La directory {dir_path} non esiste.")
```

### Utilizzando il modulo `pathlib`
```python
from pathlib import Path

# Specificare il percorso della directory
dir_path = Path("/percorso/alla/directory")

# Controllare se la directory esiste
if dir_path.is_dir():
    print(f"La directory {dir_path} esiste.")
else:
    print(f"La directory {dir_path} non esiste.")
```

### Librerie di terze parti
Sebbene la libreria standard di Python sia sufficiente per controllare se una directory esiste, librerie come `pathlib2` possono essere alternative per la consistenza attraverso le versioni di Python o per funzionalità aggiuntive.

***Nota:*** Nelle ultime versioni di Python, `pathlib` è abbastanza robusto per la maggior parte dei casi d'uso, rendendo le librerie di terze parti meno necessarie per questo specifico compito.
