---
title:                "Verifica se una directory esiste"
html_title:           "Python: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Controllare se una directory esiste è una pratica comune tra i programmatori, in quanto consente di gestire in modo efficace la gestione dei file e delle cartelle all'interno di un programma Python. Questo può evitare errori e semplificare il processo di manipolazione dei dati all'interno di una directory.

## Come fare:
```Python
import os 
# Importa il modulo os per accedere alle funzioni di sistema

if os.path.exists("directory"):
  print("La directory esiste")
else:
  print("La directory non esiste")
```
Questo semplice codice utilizza la funzione `os.path.exists()` per verificare se la directory specificata esiste o meno. Se la directory esiste, verrà stampato un messaggio di conferma, altrimenti verrà stampato un messaggio di errore.

## Approfondimento:
La verifica dell'esistenza di una directory è utile in situazioni in cui si desidera evitare la sovrascrittura di file o cartelle esistenti. Un'alternativa alla funzione `os.path.exists()` è rappresentata dalla funzione `os.path.isdir()`, che restituisce `True` se la directory esiste ed è una directory, e `False` in caso contrario. Entrambe le funzioni fanno parte del modulo `os.path` che contiene diverse altre utili funzioni per la gestione dei percorsi di sistema.

## Vedi anche:
- [Documentazione ufficiale di Python per il modulo os.path](https://docs.python.org/3/library/os.html#os.path)
- [Come accedere alle funzioni di sistema con il modulo os in Python](http://www.programmergate.com/access-system-functions-using-module-os-python/)