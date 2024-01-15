---
title:                "Verificare se una directory esiste."
html_title:           "Python: Verificare se una directory esiste."
simple_title:         "Verificare se una directory esiste."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Python, potresti doverti assicurare che una determinata directory esista prima di eseguire operazioni su di essa. Ciò potrebbe essere necessario per evitare errori o per garantire che il tuo codice funzioni correttamente.

## Come fare

Ci sono diverse possibilità per verificare l'esistenza di una directory in Python. Ecco alcuni esempi di codice che puoi utilizzare:

```Python
import os

# Metodo 1: utilizzo di os.path.exists()
if os.path.exists("path/directory"):
  print("La directory esiste!")
else:
  print("La directory non esiste.")

# Metodo 2: utilizzo di os.path.isdir()
if os.path.isdir("path/directory"):
  print("E' una directory!")
else:
  print("Non è una directory.")
```

### Esempio di output:

``` 
La directory esiste!
E' una directory!
```

In entrambi i metodi, stiamo importando il modulo "os", che contiene funzioni per l'interazione con il sistema operativo. Il primo metodo utilizza la funzione "exists()" per verificare l'esistenza di una path, mentre il secondo metodo utilizza la funzione "isdir()" per verificare se si tratta di una directory.

Esistono anche altre funzioni utili per verificare l'esistenza di una directory, come ad esempio "os.listdir()" per ottenere una lista di tutti i file e le directory presenti nella cartella specificata.

## Deep Dive

In Python, per accedere alla directory corrente in cui si sta eseguendo il codice, si può utilizzare la funzione "os.getcwd()". È anche possibile utilizzare il metodo "os.chdir()" per cambiare la directory corrente. Inoltre, è importante tenere presente che i nomi delle directory possono essere sensibili alle lettere maiuscole e minuscole, quindi è necessario prestare attenzione al modo in cui si scrive il percorso della directory quando si utilizzano le funzioni sopra menzionate.

## See Also

- [Documentazione ufficiale di Python sul modulo os](https://docs.python.org/3/library/os.html)
- [Tutorial su come utilizzare la funzione "exists()" per verificare l'esistenza di un file o directory](https://www.geeksforgeeks.org/python-os-path-exists-method/)
- [Esempi di utilizzo di "os" per gestire le directory in Python](https://stackabuse.com/introduction-to-python-os-module/)