---
title:                "Scrivere test"
html_title:           "Python: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-tests.md"
---

{{< edit_this_page >}}

# Perché

Scrivere test è fondamentale per garantire la correttezza e la stabilità di un programma. I test aiutano a identificare eventuali errori e a verificare che le modifiche apportate al codice non abbiano causato problemi in altre parti del programma.

# Come Fare

Per scrivere test in Python, è necessario utilizzare il modulo `unittest`. Di seguito un esempio di un test semplice che verifica che la funzione `add` sommi correttamente due numeri interi:

```Python
import unittest

def add(x, y):
    return x + y

class TestAdd(unittest.TestCase):

    def test_add(self):
        self.assertEqual(add(2, 3), 5)
        self.assertEqual(add(-1, 5), 4)
```

Per eseguire questo test, è possibile utilizzare il comando `python -m unittest nomefile.py` dalla riga di comando. Se non vengono sollevate eccezioni, il test viene considerato valido.

# Approfondimento

Esistono diversi tipi di test che possono essere scritti in Python, come i test di unità, di integrazione e di sistema. Inoltre, è possibile utilizzare una libreria come `coverage` per verificare la percentuale di codice testato e garantire una maggiore copertura.

# Vedi Anche

- Documentazione di `unittest`: https://docs.python.org/3/library/unittest.html
- Tutorial sui test in Python: https://realpython.com/python-testing/
- Libreria per misurare la copertura dei test: https://coverage.readthedocs.io/en/coverage-5.5/