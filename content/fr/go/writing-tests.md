---
title:                "Rédaction de tests"
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire des tests, c'est créer des petits programmes qui vérifient que le code fait ce qu'il doit faire. Les programmeurs testent pour prévenir les bugs, garantir la qualité, et simplifier la maintenance.

## How to:
Go intègre un package de test nommé `testing`. Voilà un exemple basique. 

```Go
package main

import (
    "testing"
    "strings"
)

func Saluer(nom string) string {
    return "Bonjour " + strings.Title(nom)
}

func TestSaluer(t *testing.T) {
    resultat := Saluer("monde")
    attendu := "Bonjour Monde"
    if resultat != attendu {
        t.Errorf("Saluer('monde') = %q, attendu %q", resultat, attendu)
    }
}
```

Exécutez les tests avec `go test`. Si tout va bien, il n'y a pas de sortie. Un test raté donne :

```
--- FAIL: TestSaluer (0.00s)
    main_test.go:12: Saluer('monde') = "Bonjour monde", attendu "Bonjour Monde"
FAIL
FAIL    chemin/package 0.025s
```

## Deep Dive
Le testing en Go remonte à ses débuts. `go test` compile les tests avec le code et exécute les tests. Des alternatives existent, comme Ginkgo et GoConvey, offrant des fonctionnalités supplémentaires. L'utilisation de `Table-driven tests` rend les tests organisés et couvre plus de cas.

## See Also
Pour aller plus loin :

- Document officiel pour des tests plus avancés : https://golang.org/pkg/testing/
- GoConvey pour des tests BDD : http://goconvey.co/
- Justifications et philosophies des tests : https://blog.golang.org/cover
