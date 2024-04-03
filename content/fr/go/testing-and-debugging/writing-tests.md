---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:07.495181-07:00
description: "Comment faire : En Go, les tests sont g\xE9n\xE9ralement \xE9crits dans\
  \ le m\xEAme paquet que le code qu'ils testent. Les fichiers contenant des tests\
  \ sont nomm\xE9s\u2026"
lastmod: '2024-03-13T22:44:57.137759-06:00'
model: gpt-4-0125-preview
summary: "En Go, les tests sont g\xE9n\xE9ralement \xE9crits dans le m\xEAme paquet\
  \ que le code qu'ils testent."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
En Go, les tests sont généralement écrits dans le même paquet que le code qu'ils testent. Les fichiers contenant des tests sont nommés avec le suffixe `_test.go`. Les tests sont des fonctions qui prennent un pointeur vers l'objet testing.T (du paquet `testing`) comme argument, et ils signalent un échec en appelant des méthodes telles que `t.Fail()`, `t.Errorf()`, etc.

Exemple d'un test simple pour une fonction `Add` définie dans `math.go` :
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

Fichier de test `math_test.go` :
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

Exécutez vos tests avec la commande `go test` dans le même répertoire que vos fichiers de test. Un exemple de sortie indiquant un test réussi pourrait ressembler à :

```
PASS
ok      example.com/my/math 0.002s
```

Pour les tests pilotés par des tables, qui vous permettent de tester efficacement différentes combinaisons d'entrée et de sortie, définissez une tranche de structures représentant les cas de test :

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, fonction(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("got %d, want %d", ans, tt.expected)
            }
        })
    }
}
```

## Plongée profonde
Le cadre de tests de Go, introduit dans Go 1 en même temps que le langage lui-même, a été conçu pour s'intégrer parfaitement avec la chaîne d'outils Go, reflétant l'accent mis par Go sur la simplicité et l'efficacité dans le développement logiciel. Contrairement à certains cadres de tests dans d'autres langages qui s'appuient sur des bibliothèques externes ou des configurations complexes, le paquet `testing` intégré de Go fournit une manière simple d'écrire et d'exécuter des tests.

Un aspect intéressant de l'approche de Go en matière de tests est le principe de convention sur configuration qu'il adopte, comme le modèle de nommage de fichiers (`_test.go`) et l'utilisation des fonctionnalités de la bibliothèque standard plutôt que des dépendances externes. Cette approche minimaliste encourage les développeurs à écrire des tests, car la barrière à l'entrée est faible.

Bien que les installations de test intégrées de Go couvrent beaucoup de terrain, il existe des scénarios où des outils ou des cadres tiers pourraient offrir plus de fonctionnalités, comme la génération de mock, les tests de fuzzing ou les tests de style de développement piloté par le comportement (BDD). Des bibliothèques populaires telles que Testify ou GoMock complètent les capacités de test standard de Go, offrant des assertions plus expressives ou des capacités de génération de mock, qui peuvent être particulièrement utiles dans des applications complexes avec de nombreuses dépendances.

Malgré l'existence de ces alternatives, le paquet de tests standard de Go reste la pierre angulaire des tests en Go en raison de sa simplicité, de sa performance et de son intégration étroite avec le langage et la chaîne d'outils. Que les développeurs choisissent de le compléter avec des outils tiers ou non, le cadre de tests de Go fournit une base solide pour garantir la qualité et la fiabilité du code.
