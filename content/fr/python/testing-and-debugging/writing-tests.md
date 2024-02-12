---
title:                "Rédaction de tests"
aliases: - /fr/python/writing-tests.md
date:                  2024-02-03T19:31:37.328574-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédaction de tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Écrire des tests en Python implique la création de scripts automatisés pour valider la correction de votre code. Les programmeurs font cela pour s'assurer que leurs fonctions ou leurs classes fonctionnent comme prévu sous diverses conditions, ce qui aide à détecter les erreurs tôt et facilite la maintenance et le refactoring.

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
