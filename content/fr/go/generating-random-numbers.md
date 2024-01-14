---
title:                "Go: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi
Les nombres aléatoires jouent un rôle important dans de nombreuses applications informatiques, telles que les jeux, la cryptographie et les simulations. Ils permettent de générer des valeurs aléatoires, ce qui est essentiel pour diverses fonctions et calculs.

## Comment faire
```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Générer un nombre aléatoire entre 0 et 100
	rand.Seed(time.Now().UnixNano())
	randomNum := rand.Intn(100)
	fmt.Println("Le nombre aléatoire est:", randomNum)
}
```
Sortie: Le nombre aléatoire est: 67

Dans cet exemple, nous utilisons le package intégré `math/rand` de Go pour générer un nombre aléatoire. Nous utilisons également `time` pour garantir que chaque exécution du programme génère un nombre différent. Vous pouvez également spécifier une plage de nombres en utilisant les fonctions `rand.Intn` ou `rand.Float64`.

## Plongée en profondeur
Générer des nombres aléatoires peut sembler simple, mais il existe différentes techniques pour obtenir une distribution plus uniforme ou pour créer des nombres aléatoires cryptographiquement sûrs. Le package `crypto/rand` peut être utilisé pour cela et utilise des sources de nombres aléatoires plus complexes, telles que le générateur cryptographique AES.

## Voir aussi
- [Documentation officielle Go sur les nombres aléatoires](https://golang.org/pkg/math/rand/)
- [Article sur la génération de nombres aléatoires cryptographiquement sûrs en Go](https://medium.com/coinmonks/generating-cryptographically-secure-random-numbers-in-go-4765c3c793f8)
- [Exemples de génération de nombres aléatoires en Go](https://gobyexample.com/random-numbers)