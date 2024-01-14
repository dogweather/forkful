---
title:                "Python: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-tests.md"
---

{{< edit_this_page >}}

## Perchè scrivere test è importante in Python

Scrivere test è una parte essenziale della programmazione in Python. Garantisce che il codice che scriviamo sia affidabile e funzioni correttamente. In questo post, scopriremo come scrivere test nella nostra applicazione Python.

## Come scrivere test in Python

Per scrivere test in Python, dobbiamo utilizzare il modulo `unittest` nella libreria standard di Python. Questo modulo ci permette di creare test unitari per le nostre funzioni. Vediamo un esempio di come utilizzarlo:

```Python
import unittest

def somma(numero1, numero2):
    return numero1 + numero2

class TestSomma(unittest.TestCase):

    def test_somma_positivi(self):
        self.assertEqual(somma(2, 3), 5)
    
    def test_somma_negativi(self):
        self.assertEqual(somma(-2, -5), -7)

if __name__ == '__main__':
    unittest.main()
```

In questo codice, abbiamo definito una funzione `somma` che accetta due numeri e ritorna la loro somma. Poi abbiamo creato una classe `TestSomma` che eredita dalla classe `TestCase` del modulo `unittest`. All'interno di questa classe, abbiamo definito due metodi di test: `test_somma_positivi` e `test_somma_negativi`. Utilizziamo il metodo `assertEqual` per verificare che il risultato della funzione `somma` sia corretto.

Una volta che abbiamo scritto tutti i nostri test, possiamo eseguirli eseguendo il nostro file Python, come mostrato sopra. Se tutti i test passano con successo, vedremo un output come questo:

```
..
----------------------------------------------------------------------
Ran 2 tests in 0.000s

OK
```

Se qualcosa non va durante l'esecuzione dei test, il modulo `unittest` ci fornirà informazioni dettagliate su quale test non è stato superato e perché.

## Deep Dive: Perchè scrivere test

Oltre a garantire la correttezza del nostro codice, scrivere test ha molti altri vantaggi. In primo luogo, ci aiuta a sviluppare il codice in modo più modulare, poiché dobbiamo scrivere funzioni che siano facili da testare. Inoltre, i test ci permettono di individuare eventuali bug durante lo sviluppo, che possono essere corretti prima del rilascio dell'applicazione. Infine, avere una suite completa di test ci permette di apportare modifiche al codice senza paura di rompere funzionalità esistenti, in quanto possiamo eseguire i test per verificare che tutto funzioni ancora correttamente.

## Vedi anche

- [Documentazione ufficiale sul modulo unittest](https://docs.python.org/3/library/unittest.html)
- [Cartella di esempi di test di Python](https://github.com/python/cpython/tree/master/Lib/test)
- [Articolo su Writing Effective Tests in Python](https://medium.com/@vladubogdan/writing-effective-tests-in-python-bd2146ac81e)