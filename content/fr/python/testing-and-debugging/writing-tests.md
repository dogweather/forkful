---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:37.328574-07:00
description: "Comment faire : Python est livr\xE9 avec un module int\xE9gr\xE9 pour\
  \ \xE9crire des tests appel\xE9 `unittest`. Voici comment vous pouvez l'utiliser\
  \ pour tester une\u2026"
lastmod: '2024-03-13T22:44:57.239901-06:00'
model: gpt-4-0125-preview
summary: "Python est livr\xE9 avec un module int\xE9gr\xE9 pour \xE9crire des tests\
  \ appel\xE9 `unittest`."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
Python est livré avec un module intégré pour écrire des tests appelé `unittest`. Voici comment vous pouvez l'utiliser pour tester une fonction simple :

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Devrait être 12")

if __name__ == '__main__':
    unittest.main()
```

Lorsque vous exécutez ce script de test, vous devriez voir une sortie indiquant que vos tests ont réussi (ou échoué).

Pour des tests plus modernes et expressifs, vous pouvez utiliser une bibliothèque tierce comme `pytest`. Tout d'abord, vous devrez l'installer en utilisant pip :

```shell
pip install pytest
```

Ensuite, vous pouvez écrire vos tests d'une manière plus simple sans avoir besoin de sous-classer quoi que ce soit :

```python
# Sauvegardez ceci dans un fichier nommé test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Devrait être 12"
```

Pour exécuter vos tests avec `pytest`, exécutez simplement :

```shell
pytest test_with_pytest.py
```

Vous devriez voir la sortie de pytest montrant vos résultats de tests.
