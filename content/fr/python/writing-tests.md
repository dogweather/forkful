---
title:                "Python: Ecrire des tests"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi

Les tests sont un élément essentiel du développement logiciel. Ils permettent de vérifier si le code fonctionne correctement et s'il répond aux exigences attendues. En écrivant des tests, vous pouvez vous assurer que votre code est robuste, fiable et sans bugs.

# Comment faire

Pour écrire des tests en Python, vous pouvez utiliser le module intégré `unittest`. Voici un exemple simple de test unitaire qui vérifie si une fonction calcule correctement la moyenne d'une liste de nombres :

```Python
import unittest

def moyenne(liste):
    return sum(liste) / len(liste)

class TestMoyenne(unittest.TestCase):
    def test_calcule_moyenne(self):
        resultat = moyenne([2, 4, 6, 8])
        self.assertEqual(resultat, 5)
```
La première ligne importe le module `unittest`, puis on définit une fonction `moyenne()` qui calcule la moyenne d'une liste. Ensuite, on crée une classe `TestMoyenne` qui hérite de la classe `unittest.TestCase`, ce qui nous permet de définir des méthodes de test. La méthode `test_calcule_moyenne()` fait appel à notre fonction `moyenne()` avec une liste de nombres et vérifie si le résultat est bien égal à 5 avec la méthode `assertEqual()`. 

Vous pouvez ensuite exécuter votre test en utilisant la commande `python -m unittest` dans votre terminal. Si tout est correct, vous devriez voir un message indiquant que votre test a réussi.

# Plongée en profondeur

Écrire des tests n'est pas seulement utile pour vérifier si votre code fonctionne, mais c'est également un moyen efficace de détecter d'éventuels bugs dans votre code. Les tests peuvent vous aider à identifier rapidement les problèmes et à les corriger avant qu'ils ne deviennent plus complexes et difficiles à résoudre.

Il est également important de faire des tests régulièrement. En utilisant des outils tels que `unittest` ou `pytest`, vous pouvez automatiser vos tests et les exécuter chaque fois que vous apportez des modifications à votre code. Cela vous permet de vous assurer que ces modifications n'ont pas affecté le comportement du code existant.

De plus, les tests peuvent également améliorer la qualité de votre code en imposant des normes et des bonnes pratiques de programmation. En écrivant des tests, vous êtes obligés de penser de manière critique à votre code et à la manière dont il doit fonctionner, ce qui peut conduire à un code plus propre et plus bien conçu.

# Voir aussi

- [Documentation du module unittest en français](https://docs.python.org/fr/3/library/unittest.html)
- [Un guide complet pour écrire des tests en Python](https://realpython.com/python-testing/)
- [Livre "Test Driven Development with Python" de Harry Percival](https://www.obeythetestinggoat.com/)