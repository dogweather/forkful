---
title:                "Écrire des tests"
html_title:           "Go: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire des tests est essentiel pour garantir la qualité et la stabilité du code. Les tests aident à détecter et à corriger les bogues avant qu'ils n'affectent les utilisateurs finaux, ce qui permet d'économiser du temps et de l'argent à long terme.

## Comment faire

Ecrire des tests en Go est simple et efficace grâce à son intégration native avec le package de tests standard. Voici un exemple de test simple pour une fonction qui retourne la somme de deux nombres :

```Go
func Sum(a, b int) int {
    return a + b
}

func TestSum(t *testing.T) {
    result := Sum(2, 3)
    expected := 5

    if result != expected {
        t.Errorf("Expected %d, but received %d", expected, result)
    }
}
```

L'utilisation de "testing.T" permet d'effectuer des assertions pour vérifier si les résultats sont conformes aux attentes. En exécutant "go test" dans le terminal, le test sera exécuté et le résultat sera affiché : "PASS".

## Approfondissement

Ecrire des tests en Go offre une grande flexibilité. Les tests peuvent être organisés en différents fichiers, ce qui facilite la maintenance à mesure que le code s'agrandit. De plus, les tests peuvent être exécutés en parallèle, ce qui permet de gagner du temps lors de la phase de test.

Pour écrire des tests plus complexes, Go offre des fonctionnalités telles que les table-driven tests, qui permettent de tester plusieurs valeurs d'entrée avec les mêmes assertions. Les sous-tests sont également utiles pour tester plusieurs scénarios avec différentes mises en place de données.

## Voir aussi

- [Documentation de Go sur le package de tests](https://golang.org/pkg/testing/)
- [Guide pour écrire des tests en Go](https://blog.golang.org/subtests)
- [Table-driven tests en Go](https://medium.com/@quii/learn-go-by-writing-tests-345b0c0c0d8b)