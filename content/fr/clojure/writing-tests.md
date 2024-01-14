---
title:    "Clojure: Écrire des tests"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

L'écriture de tests est une pratique essentielle pour tout programmeur. Elle permet de vérifier si le code fonctionne correctement, de détecter les erreurs potentielles et de faciliter les modifications ultérieures. En d'autres termes, les tests garantissent la qualité du code et peuvent vous faire gagner un temps précieux lors du développement.

## Comment faire

Pour écrire des tests en Clojure, vous pouvez utiliser l'outil intégré appelé clojure.test. Il vous permet de créer des cas de test avec la fonction `deftest` et d'effectuer des assertions avec `is`. Voyons un exemple simple où nous testons une fonction qui renvoie la somme de deux nombres :

```Clojure
(deftest somme-test
  (is (= 4 (somme 2 2)))
  (is (= 10 (somme 5 5)))
)
```

Ici, nous avons créé un cas de test appelé "somme-test" avec deux assertions `is`. La première vérifie si la somme de 2 et 2 est égale à 4, et la seconde vérifie si la somme de 5 et 5 est égale à 10. Pour exécuter ces tests, nous utilisons la fonction `run-tests`.

```Clojure
(run-tests)
```

Et voici le résultat :

```
Testing user
Ran 1 tests containing 2 assertions.
0 failures, 0 errors.
```

Nous pouvons voir que nos tests ont été réussis. Cependant, si nous avions fait une erreur dans notre fonction `somme`, par exemple en utilisant le mauvais opérateur, les tests auraient échoué et nous aurions pu corriger notre erreur immédiatement.

## Plongée en profondeur

Il existe d'autres outils et bibliothèques en Clojure pour écrire des tests tels que midje, which, et cucumber. Chacun a ses avantages et ses inconvénients, il est donc important d'explorer et de trouver celui qui convient le mieux à votre style de développement.

N'oubliez pas que les tests doivent être écrits pour chaque nouvelle fonctionnalité ou modification de code. De plus, les tests devraient être automatisés et exécutés fréquemment pour garantir une qualité de code constante.

## Voir aussi

- [Documentation officielle de clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Un guide complet sur l'écriture de tests en Clojure](https://www.braveclojure.com/testing/)
- [Comparaison des différents outils de test en Clojure](https://medium.com/@randycoulman/choosing-a-clojure-testing-tool-85d16753a227)