---
title:                "Python: Scrivere test"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-tests.md"
---

{{< edit_this_page >}}

## Perché
Scrivere test è fondamentale per garantire la correttezza e l'affidabilità del nostro codice. Non solo ci aiuta a trovare errori durante lo sviluppo, ma ci permette anche di refactorizzare il codice con più sicurezza.

## Come fare
Per scrivere test in Python, possiamo utilizzare il modulo `unittest` della libreria standard. Iniziamo importando il modulo:

```
import unittest
```

Definiamo poi una classe che erediti da `unittest.TestCase`. All'interno di questa classe, possiamo definire i nostri test come metodi che iniziano con il prefisso `test_`:

```
class CalcolatriceTest(unittest.TestCase):
    def test_somma(self):
        somma = 3 + 4
        self.assertEqual(somma, 7)

    def test_prodotto(self):
        prodotto = 5 * 2
        self.assertEqual(prodotto, 10)

if __name__ == '__main__':
    unittest.main()
```

Qui stiamo testando una semplice calcolatrice, confrontando il risultato delle operazioni con il valore atteso utilizzando il metodo `assertEqual`. Infine, eseguiamo i nostri test con il metodo `main` di `unittest`.

## Approfondimento
Esistono varie tipologie di test che possiamo scrivere in Python, come test di unità, test di integrazione e test funzionali. Inoltre, possiamo utilizzare anche altre librerie per il testing, come `pytest` e `doctest`. È importante scrivere test significativi e leggibili, in modo da facilitare la manutenzione del codice.

## Vedi anche
- Documentazione ufficiale del modulo `unittest`: https://docs.python.org/3/library/unittest.html
- Tutorial sul testing in Python: https://realpython.com/python-testing/
- Video tutorial sui test di unità in Python: https://www.youtube.com/watch?v=1Lfv5tUGsn8