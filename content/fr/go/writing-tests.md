---
title:                "Go: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire des tests peut sembler fastidieux et prendre du temps, mais cela peut en réalité être bénéfique pour les développeurs Go. Les tests aident à détecter les problèmes potentiels dès le début du processus de développement, ce qui peut finalement gagner du temps et de l'effort à long terme.

## Comment faire

Ecrire des tests en Go est relativement simple. Tout d'abord, vous devez créer un fichier de test en utilisant un nom de fichier avec le suffixe `_test.go`. Ensuite, vous pouvez écrire des fonctions de test en utilisant le framework de tests intégré de Go. Voici un exemple de fonction de test :

```Go
func TestCalculer(t *testing.T) {
    résultat := calculer(5, 10)
    if résultat != 15 {
        t.Errorf("Résultat incorrect, attendu : %d, obtenu : %d", 15, résultat)
    }
}
```

Cet exemple teste une fonction "calculer" qui prend deux entiers en paramètres et renvoie leur somme. Le framework de tests de Go utilise la fonction "T.Error" pour signaler une erreur si le résultat obtenu ne correspond pas au résultat attendu. Vous pouvez également utiliser `T.Fail` pour signaler un test en échec s'il y a une erreur de logique dans votre fonction de test.

## Plongée en profondeur

Il est important de noter que les tests en Go sont exécutés en parallèle, ce qui peut être utile pour accélérer l'exécution des tests. De plus, vous pouvez utiliser l'option -count pour exécuter le même test plusieurs fois, ce qui peut être utile pour détecter les fuites de mémoire et autres problèmes de performances.

Il est également possible d'écrire des tests de table pour tester plusieurs cas avec différentes entrées et résultats attendus. Cette approche est utile pour tester les fonctions ayant plusieurs chemins d'exécution possibles.

## Voir aussi

Voici quelques liens utiles pour en savoir plus sur l'écriture des tests en Go :

- [Documentation officielle sur le framework de tests de Go](https://pkg.go.dev/testing)
- [Tutoriel sur les tests en Go](https://www.digitalocean.com/community/tutorials/how-to-write-unit-tests-in-go)

Maintenant que vous savez comment écrire des tests en Go, n'hésitez pas à les incorporer dans votre processus de développement pour améliorer la qualité de votre code et gagner du temps à long terme !