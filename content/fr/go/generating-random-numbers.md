---
title:                "Go: Production de nombres aléatoires"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires est un outil essentiel pour de nombreux programmes informatiques, notamment pour les jeux, les simulations et les tests de performances. Cela permet de créer des scénarios variés et imprévisibles, tout en garantissant des résultats cohérents et répétables.

## Comment faire

Pour générer des nombres aléatoires en Go, il est nécessaire d'utiliser la bibliothèque "math/rand" et "time". Tout d'abord, il faut initialiser une source de "graine" en utilisant la fonction "time.Now().UnixNano()", qui utilise l'horloge système pour générer une valeur aléatoire. Ensuite, cette source peut être utilisée pour initialiser un générateur de nombres aléatoires en utilisant la fonction "rand.NewSource()". Enfin, on utilise la fonction "rand.Intn()" pour générer un nombre aléatoire entre 0 et un nombre maximum spécifié.

Voici un exemple de code pour générer 10 nombres aléatoires compris entre 1 et 100 :

```
package main

import (
  "fmt"
  "math/rand"
  "time"
)

func main() {
  rand.Seed(time.Now().UnixNano())
  for i := 0; i < 10; i++ {
    num := rand.Intn(100) + 1
    fmt.Printf("%d\n", num)
  }
}
```

Voici un exemple de sortie possible :

```
57
12
98
23
86
39
41
6
74
88
```

## Profondeur

En plus de générer des nombres aléatoires simples, la bibliothèque "math/rand" offre également des fonctions pour générer des nombres flottants et des strings aléatoires. Il est également possible de personnaliser la source de graine pour obtenir des séquences de nombres plus uniques. Il est important de noter que les nombres générés sont pseudo-aléatoires, c'est-à-dire qu'ils sont déterminés par une série de calculs à partir de la source de graine.

## Voir aussi

- Documentation officielle de la bibliothèque "math/rand" : https://golang.org/pkg/math/rand/
- Tutoriel sur la génération de nombres aléatoires en Go : https://dev.to/rakyll/how-to-generate-random-numbers-in-go-from-neverland-1bk
- Exemples de projets utilisant la génération de nombres aléatoires en Go : https://awesome-go.com/#random-numbers