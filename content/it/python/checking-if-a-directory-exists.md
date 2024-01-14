---
title:    "Python: Verificare se esiste una cartella"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

Se stai programmando in Python, potrebbe essere necessario verificare se una determinata directory esiste o meno. Questo è particolarmente utile se il tuo programma deve gestire file e cartelle in un determinato percorso. In questo articolo esploreremo come verificare l'esistenza di una directory utilizzando Python.

## Come fare

Per verificare se una directory esiste, useremo la funzione `path.exists()` del modulo `os` di Python. Questa funzione accetta come argomento il percorso della directory e restituisce un valore booleano, `True` se la directory esiste e `False` se non esiste. Vediamo un esempio di codice:

```Python
import os

# Definiamo il percorso della directory da verificare
directory = "/percorso/della/directory"

# Utilizziamo la funzione path.exists()
if os.path.exists(directory):
    print("La directory esiste")
else:
    print("La directory non esiste")
```

Se la directory specificata esiste, il programma stamperà "La directory esiste". In caso contrario, stamperà "La directory non esiste".

Un'altra opzione è utilizzare la funzione `path.isdir()` per verificare se il percorso specificato è una directory o no. Questa funzione restituisce sempre un valore booleano, `True` se si tratta di una directory e `False` se non lo è. Ad esempio:

```Python
import os

# Definiamo il percorso della directory da verificare
directory = "/percorso/della/directory"

# Utilizziamo la funzione path.isdir()
if os.path.isdir(directory):
    print("Il percorso specificato è una directory")
else:
    print("Il percorso specificato non è una directory")
```

Entrambe queste funzioni sono utili per verificare l'esistenza di una directory e possono essere utilizzate a seconda delle tue esigenze.

## Approfondimento

Oltre alle funzioni menzionate sopra, Python offre anche il modulo `pathlib` che fornisce una serie di metodi per gestire percorsi e file in modo più intuitivo. Ad esempio, per verificare se una directory esiste, possiamo utilizzare il metodo `Path.is_dir()` come mostrato di seguito:

```Python
from pathlib import Path

# Definiamo il percorso della directory da verificare
directory = Path("/percorso/della/directory")

# Utilizziamo il metodo is_dir()
if directory.is_dir():
    print("La directory esiste")
else:
    print("La directory non esiste")
```

Il modulo `pathlib` offre anche una maggiore flessibilità nella gestione dei percorsi e dei file e ti consente di svolgere diverse operazioni senza dover importare altri moduli.

## Vedi anche

- Documentazione ufficiale Python per il modulo `os`: https://docs.python.org/3/library/os.html
- Documentazione ufficiale Python per il modulo `pathlib`: https://docs.python.org/3/library/pathlib.html