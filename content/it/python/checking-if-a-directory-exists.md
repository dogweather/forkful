---
title:    "Python: Verifica dell'esistenza di una directory"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché controllare se una directory esiste

Controllare se una directory esiste è un'operazione molto utile quando si lavora con file e directory in Python. Questa verifica è particolarmente importante quando si deve leggere o scrivere file in una specifica directory e si vuole evitare errori durante l'esecuzione del programma.

## Come fare

Per controllare se una directory esiste in Python, si può utilizzare la funzione `os.path.isdir()`. Questa funzione prende come parametro il percorso della directory e restituisce `True` se la directory esiste e `False` se non esiste.

Ecco un esempio di codice che utilizza questa funzione:

```Python
import os

directory = "/percorso/alla/directory"

if os.path.isdir(directory):
    print("La directory esiste!")
else:
    print("La directory non esiste.")
```

In questo esempio, si importa il modulo `os` e si assegna alla variabile `directory` il percorso alla directory che si desidera controllare. Successivamente, viene utilizzata la funzione `isdir()` per controllare se la directory esiste e viene stampato un messaggio di conseguenza.

## Analisi approfondita

In effetti, la funzione `isdir()` fa più di un semplice controllo di esistenza. Se il percorso passato come parametro corrisponde a un file o a una directory, la funzione restituirà comunque `False`. Inoltre, questa funzione tiene conto di eventuali errori di sistema e restituirà `False` anche in quei casi.

In sostanza, la funzione `isdir()` è un modo sicuro per controllare se una directory esiste senza incorrere in errori o problemi di sistema.

## Vedi anche
- [Documentazione ufficiale di Python su `os.path.isdir()`](https://docs.python.org/3/library/os.path.html#os.path.isdir)
- [Come creare una directory in Python](https://www.pythonforbeginners.com/files/creating-directories)
- [Come leggere un file in Python](https://www.geeksforgeeks.org/reading-writing-text-files-python/)