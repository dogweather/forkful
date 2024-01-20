---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Générer des nombres aléatoires est une tâche commune en programmation. Cela permet d'ajouter de l'incertitude dans nos programmes, nécessaire pour les jeux, la simulation, le chiffrement et bien plus.

## Comment faire
Voici un exemple de code pour générer des nombres aléatoires en Go.

```Go
package main

import (
  "fmt"
  "math/rand"
  "time"
)

func main() {
  rand.Seed(time.Now().UnixNano())
  num := rand.Intn(100) // génère un nombre entre 0 et 99

  fmt.Println(num)
}
```

La sortie sera un nombre aléatoire entre 0 et 99.

## Plongée profonde
Historiquement, générer de l'incertitude en programmation n'était pas si simple qu'aujourd'hui, impliquant l'utilisation de générateurs de nombres pseudo-aléatoires qui ont besoin d'être "semés" pour éviter les répétitions.

En Go, `math/rand` est utilisé pour générer des nombres pseudo-aléatoires. Il utilise le temps actuel, en nano-secondes depuis 1970, comme graine pour `Rand.Seed()`. Toutefois, il faut noter que ces nombres ne sont pas adaptés à la cryptographie pour laquelle on utilisera `crypto/rand` à la place.

Une alternative est d'utiliser l'API de cryptographie pour générer des nombres aléatoires cryptographiquement sécurisés, mais cela peut être excessif pour certaines applications.

## Voir aussi
Pour approfondir, voici quelques liens utiles:

- Documentation de math/rand: https://golang.org/pkg/math/rand/
- Guide complet sur les nombres aléatoires en Go: https://yourbasic.org/golang/generate-random-number/
- Sécurité de la cryptographie et génération de nombres aléatoires: https://arxiv.org/pdf/2001.00939.pdf