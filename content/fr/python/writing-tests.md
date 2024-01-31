---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"

category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Écrire des tests, c'est vérifier que chaque pièce de ton code fonctionne comme prévu. Les développeurs font ça pour éviter les bugs, faciliter les mises à jour et dormir tranquilles.

## How to: (Comment faire :)
Python utilise `unittest` pour les tests. Voici un exemple simple. Imagine un fichier `maths.py` avec la fonction `addition(x, y)`:

```python
# maths.py
def addition(x, y):
    return x + y
```

Crée un test dans `test_maths.py`:

```python
# test_maths.py
import unittest
from maths import addition

class TestMaths(unittest.TestCase):
    def test_addition(self):
        self.assertEqual(addition(3, 4), 7)

if __name__ == '__main__':
    unittest.main()
```

Exécute le test:

```
python -m unittest test_maths
```

Si tout va bien, tu verras:

```
.
----------------------------------------------------------------------
Ran 1 test in 0.001s

OK
```

## Deep Dive (Plongée en profondeur)
Avant `unittest`, les gens imprimaient des valeurs, les comparant manuellement - c'était le chaos. D'autres options incluent `pytest` - très populaire, et `nose`. En interne, `unittest` crée une instance de la classe `TestCase` pour chaque test, exécute la méthode et compare le résultat.

## See Also (Voir aussi)
Pour plus d'info, consulte:
- La documentation Python sur `unittest`: https://docs.python.org/3/library/unittest.html
- Un guide pour `pytest`: https://docs.pytest.org/
- Plus sur l'histoire des tests: https://en.wikipedia.org/wiki/Test-driven_development
