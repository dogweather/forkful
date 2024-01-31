---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"

category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-tests.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Scrivere test significa creare script per verificare che il codice funzioni come previsto. I programmatori scrivono test per garantire che il codice sia affidabile e per prevenire bug quando si aggiorna o si modifica il codice.

## Come fare:
Ecco un esempio di test unitario con `unittest`, il framework di testing integrato in Python:

```Python
import unittest

def somma(x, y):
    return x + y

class TestSomma(unittest.TestCase):
    def test_somma_positivi(self):
        self.assertEqual(somma(1, 2), 3)
    
    def test_somma_negativi(self):
        self.assertEqual(somma(-1, -1), -2)
    
    def test_somma_misti(self):
        self.assertEqual(somma(-1, 2), 1)

if __name__ == '__main__':
    unittest.main()
```
Output presunto:
```
...
----------------------------------------------------------------------
Ran 3 tests in 0.001s

OK
```

## Approfondimenti:
I test automatici hanno origine negli anni '60 ma hanno guadagnato popolarità con lo sviluppo delle metodologie Agile e TDD (Test-Driven Development). Oltre a `unittest`, esistono altre librerie di test come `pytest` e `nose`, che offrono funzionalità aggiuntive. Un dettaglio chiave nella scrittura di test è il concetto di "mocking", ovvero simulare parti del sistema che non si vuole testare direttamente o che sono esterne al contesto del test.

## Vedi anche:
Per approfondire, visita i seguenti link:

- La documentazione ufficiale di Python su unittest: https://docs.python.org/3/library/unittest.html
- Una guida a `pytest`, un framework di testing più potente e flessibile: https://docs.pytest.org/en/stable/
- TDD (Test Driven Development) su Wikipedia: https://it.wikipedia.org/wiki/Test_driven_development
