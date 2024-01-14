---
title:    "Go: Écriture de tests"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en programmation Go ?

Les tests sont un élément crucial du développement logiciel, car ils permettent de vérifier si le code fonctionne correctement et de détecter les éventuelles erreurs. C'est particulièrement important en programmation Go, car ce langage met l'accent sur la fiabilité et la stabilité du code. Écrire des tests en Go est donc essentiel pour garantir la qualité de votre code et éviter les bugs.

## Comment écrire des tests en Go ?

Pour écrire des tests en Go, vous pouvez utiliser le package intégré "testing". Voici un exemple de test unitaire pour une fonction qui calcule la moyenne de deux nombres :

```
func TestAverage(t *testing.T) {
    result := average(5, 10)
    if result != 7.5 {
        t.Errorf("Expected result to be 7.5, got %v instead", result)
    }
}
```

Dans cet exemple, nous avons créé une fonction de test nommée "TestAverage" qui utilise la fonction "average" à tester et vérifie si le résultat correspond à nos attentes. Nous utilisons également la fonction "t.Errorf" pour afficher un message d'erreur en cas d'échec du test.

Vous pouvez également utiliser des tests de tableaux pour tester plusieurs cas avec une seule fonction de test. Par exemple :

```
func TestAverageTable(t *testing.T) {
    var tests = []struct {
        input1 int
        input2 int
        expected float64
    }{
        {5, 10, 7.5},
        {0, 0, 0},
        {10, 10, 10},
    }
    for _, test := range tests {
        result := average(test.input1, test.input2)
        if result != test.expected {
            t.Errorf("Expected result to be %v, got %v instead", test.expected, result)
        }
    }
}
```

Dans cet exemple, nous avons créé un tableau contenant plusieurs cas de tests avec différentes valeurs d'entrée et de sortie attendues. La boucle "for" nous permet de tester chaque cas avec une seule fonction de test. Cela rend l'écriture de tests plus efficace et facile à maintenir.

## Approfondissement sur l'écriture de tests en Go

En plus des tests unitaires, vous pouvez également écrire des tests d'intégration ou des tests de performance en utilisant des packages tels que "net/http/httptest" ou "testing/quick". Vous pouvez également utiliser des outils de couverture de code pour vérifier la qualité de vos tests et vous assurer que toutes les parties de votre code sont testées.

Une bonne pratique en programmation Go est de commencer par écrire les tests avant le code lui-même. Cela vous aidera à mieux comprendre les fonctionnalités que vous devez implémenter et à vous assurer que votre code est testable dès le début.

# Voir aussi

- Documentation officielle de Go sur les tests : https://golang.org/pkg/testing/
- Tutoriel sur les tests en Go : https://blog.alexellis.io/golang-writing-unit-tests/
- Article sur les bonnes pratiques en matière de tests en Go : https://medium.com/@flaviocopes/go-testing-part-i-e746e3b8df14