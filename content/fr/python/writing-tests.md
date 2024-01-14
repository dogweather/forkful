---
title:    "Python: Rédaction de tests"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi 
Les tests sont un élément essentiel de la programmation en Python, ils permettent de s'assurer que le code fonctionne correctement et de détecter rapidement les erreurs. Les tests garantissent également que les modifications apportées au code ne causent pas de régressions.

## Comment Faire 
Pour écrire des tests en Python, il suffit d'utiliser le module intégré `unittest`, qui fournit des classes et des méthodes pour créer et exécuter des tests. Voici un exemple de code utilisant `unittest` pour tester une fonction qui calcule la moyenne d'une liste de nombres :

```Python
import unittest 

# Définir la fonction à tester 
def moyenne(liste):
    return sum(liste) / len(liste)

# Sous-classe de TestCase pour créer un test
class TestMoyenne(unittest.TestCase):

    # Définir un test avec une méthode nommée "test_<nom du test>"  
    def test_moyenne(self):
    
        # Données d'entrée et résultat attendu
        liste = [1, 2, 3, 4]
        resultat = 2.5
        
        # Utiliser la méthode assertEqual pour vérifier le résultat
        self.assertEqual(moyenne(liste), resultat)
        
# Exécuter les tests en utilisant la méthode main() du module unittest 
if __name__ == '__main__':
    unittest.main()
```

Lorsque vous exécutez ce code, vous devriez voir une sortie comme ceci :

```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

Ce qui signifie que le test a réussi. Si vous modifiez la fonction `moyenne` pour retourner un résultat incorrect, le test échouera et affichera un message d'erreur vous indiquant l'endroit où l'erreur s'est produite.

## Plongée en Profondeur 
Outre l'utilisation de `unittest`, il existe différentes méthodes pour écrire des tests en Python, telles que le module `pytest` ou encore le concept de TDD (Test Driven Development). Il est également important de suivre les bonnes pratiques pour écrire des tests efficaces, tels que les noms clairs et explicites pour les fonctions de test et l'utilisation de plusieurs cas de test pour couvrir toutes les situations possibles.

## Voir Aussi 
- [Documentation officielle de `unittest`](https://docs.python.org/fr/3/library/unittest.html)
- [Guide sur les tests en Python](https://realpython.com/python-testing/)
- [Tutoriel sur TDD en Python](https://www.testdriven.io/blog/tdd-in-python/)
- [Documentation officielle de `pytest`](https://docs.pytest.org/en/6.2.x/)