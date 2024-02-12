---
title:                "Scrivere test"
aliases:
- /it/python/writing-tests.md
date:                  2024-02-03T19:31:25.413069-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere test"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è e perché?
Scrivere test in Python comporta la creazione di script automatizzati per validare la correttezza del tuo codice. I programmatori lo fanno per assicurarsi che le loro funzioni o classi funzionino come previsto in varie condizioni, il che aiuta a individuare gli errori precocemente e facilita la manutenzione e il rifattorizzamento più semplici.

## Come fare:
Python include un modulo integrato per scrivere test chiamato `unittest`. Ecco come puoi usarlo per testare una semplice funzione:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Dovrebbe essere 12")

if __name__ == '__main__':
    unittest.main()
```

Quando esegui questo script di test, dovresti vedere un output che indica che i tuoi test sono stati superati (o falliti).

Per test più moderni ed espressivi, puoi usare una libreria di terze parti come `pytest`. Prima, dovrai installarla usando pip:

```shell
pip install pytest
```

Poi, puoi scrivere i tuoi test in un modo più semplice senza dover sottoclassificare nulla:

```python
# Salva questo in un file chiamato test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Dovrebbe essere 12"
```

Per eseguire i tuoi test con `pytest`, esegui semplicemente:

```shell
pytest test_with_pytest.py
```

Dovresti vedere l'output di pytest che mostra i risultati dei tuoi test.
