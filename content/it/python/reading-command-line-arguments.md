---
title:                "Python: Lettura degli argomenti della riga di comando"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Perché qualcuno dovrebbe leggere gli argomenti della riga di comando in Python? Bene, spesso può essere utile per fare del codice più flessibile, permettendo agli utenti di fornire input personalizzati senza dover modificare direttamente il codice sorgente.

## Come Fare

Per leggere gli argomenti della riga di comando in Python, è necessario utilizzare il modulo `sys` che fornisce informazioni sul sistema e su come viene eseguito il codice Python. In particolare, la lista `sys.argv` contiene tutti gli argomenti forniti dall'utente quando il programma è stato avviato. Vediamo un esempio:

```
# Codice Python per la lettura degli argomenti della riga di comando

import sys

# Stampa la lista degli argomenti forniti
print("Gli argomenti forniti sono:", sys.argv)

# Stampa il numero totale degli argomenti
print("Il numero totale degli argomenti è:", len(sys.argv))

# Stampa il primo argomento (di solito il nome del file)
print("Il primo argomento è:", sys.argv[0])

# Stampa il secondo argomento (se presente)
if len(sys.argv) > 1:
    print("Il secondo argomento è:", sys.argv[1])
```

Se eseguiamo questo codice con `python script.py arg1 arg2`, otterremo il seguente output:

```
Gli argomenti forniti sono: ['script.py', 'arg1', 'arg2']
Il numero totale degli argomenti è: 3
Il primo argomento è: script.py
Il secondo argomento è: arg1
```

## Approfondimento

Oltre alla lista degli argomenti forniti dall'utente, il modulo `sys` ci fornisce anche altre informazioni utili come:

- `sys.argv[0]`: il nome del file eseguito
- `sys.stderr`: l'oggetto stream per la stampa degli errori
- `sys.getdefaultencoding()`: l'encoding predefinito del sistema
- E molti altri!

Per ulteriori informazioni, si consiglia di leggere la documentazione ufficiale del modulo `sys`.

## Vedi Anche

- [Documentazione ufficiale del modulo `sys`](https://docs.python.org/3/library/sys.html)
- [Come leggere e analizzare gli argomenti della riga di comando in Python](https://www.tutorialspoint.com/python/python_command_line_arguments.htm)
- [Come utilizzare il modulo `argparse` per creare una interfaccia a riga di comando in Python](https://realpython.com/command-line-interfaces-python-argparse/)