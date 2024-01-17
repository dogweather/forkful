---
title:                "Écrire des tests"
html_title:           "Python: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-tests.md"
---

{{< edit_this_page >}}

# Qu'est-ce que les tests de code et pourquoi en faire?

Les tests de code sont des morceaux de code écrits spécifiquement pour vérifier si une autre partie de code fait ce qu'elle est censée faire. Les programmeurs font des tests pour s'assurer que leur code fonctionne correctement et pour éviter les erreurs et les bugs.

# Comment faire:

```python
def sum(a, b):
  return a + b
# Exemple de test pour la fonction de somme
# On s'attend à ce que 2+2 soit égal à 4
assert sum(2, 2) == 4
```

Les tests peuvent également être organisés en modules ou classes pour tester différentes parties de code. Voici un autre exemple:

```python
# Une classe pour tester une calculatrice
class TestCalculator:

  def test_addition(self):
    assert sum(5, 10) == 15

  def test_subtraction(self):
    assert subtract(10, 5) == 5

  def test_multiplication(self):
    assert multiply(2, 3) == 6
```
# Plongée en profondeur:

Les tests de code ont été utilisés depuis les premiers jours de la programmation, car ils permettent de détecter les erreurs dès leur apparition et de les corriger avant qu'elles ne se propagent. Les alternatives aux tests incluent la revue de code par des pairs et l'utilisation de l'intégration continue pour détecter les erreurs à mesure qu'elles sont ajoutées au code.

Les tests de code peuvent également être écrits à l'aide de différentes bibliothèques et frameworks, tels que unittest, pytest ou nose. Chacun a ses propres avantages et il est important de choisir celui qui convient le mieux à votre projet.

# Voir aussi:

Vous pouvez en apprendre plus sur les tests de code en consultant ces sources:
- [Python Testing 101](https://realpython.com/python-testing/)
- [Writing Tests in Python](https://docs.python.org/3/library/unittest.html)
- [Pytest Documentation](https://docs.pytest.org/en/stable/)