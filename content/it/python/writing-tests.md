---
title:    "Python: Scrivere test"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test è importante in Python
Scrivere test è un componente fondamentale della programmazione in Python. I test ci aiutano a verificare che il nostro codice funzioni correttamente e ci permettono di identificare eventuali errori o bug. Inoltre, ci aiutano a sviluppare codice più robusto e manutenibile nel tempo. 

## Come scrivere test in Python
Per iniziare a scrivere test in Python, dobbiamo utilizzare il modulo di testing integrato, chiamato `unittest`. Con questo modulo, possiamo definire delle classi di test che controllano il comportamento del nostro codice. Di seguito, un esempio di un test per una semplice funzione di addizione:

```Python
import unittest

def add(x, y):
    return x + y

class TestAdd(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(2, 3), 5)
        self.assertEqual(add(-1, 5), 4)

if __name__ == '__main__':
    unittest.main()
```

Nell'esempio sopra, abbiamo definito una classe di test chiamata `TestAdd` che eredita dalla classe `unittest.TestCase`. All'interno di questa classe, abbiamo definito un metodo chiamato `test_add` che contiene gli assert per verificare che la nostra funzione `add` funzioni come previsto. Infine, abbiamo utilizzato `unittest.main()` per eseguire i nostri test.

## Approfondimento su come scrivere test in Python
Oltre alla semplice sintassi per scrivere test, esistono anche diverse pratiche consigliate da seguire quando si scrivono test in Python. Alcune di queste includono l'organizzazione dei test in moduli separati, l'utilizzo di tecniche di test-driven development e l'uso di strumenti di code coverage per identificare il codice non testato. Inoltre, ci sono anche diversi framework di testing di terze parti che possono aiutarci a scrivere test più avanzati e completi.

## Vedi anche
- [Documentazione ufficiale di `unittest`](https://docs.python.org/3/library/unittest.html)
- [Python Testing with pytest](https://realpython.com/python-testing/)
- [The Hitchhiker's Guide to Python Testing](https://docs.python-guide.org/writing/tests/)