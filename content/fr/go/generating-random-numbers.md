---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:12.837083-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? 
Générer des nombres aléatoires, c'est comme lancer des dés avec votre ordinateur. Les programmeurs utilisent l'aléatoire pour tout, de la simulation à la sélection d'éléments en passant par la sécurité informatique.

## How to:
Pour générer un nombre aléatoire en Go, on utilise le package `math/rand`. Voici les bases :

```go
package main

import (
	"fmt"
    "math/rand"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano()) // Initialisation avec une graine variable
    randomNumber := rand.Intn(100)   // Génère un nombre aléatoire entre 0 et 99
    fmt.Println(randomNumber)
}
```

Si vous exécutez ce code, vous aurez un nombre différent à chaque fois. Essayez !

## Deep Dive
Historiquement, générer de l'aléatoire était un vrai casse-tête. Les premiers ordinateurs utilisaient des phénomènes physiques pour produire de l'aléatoire. Aujourd'hui, on utilise des algorithmes dits pseudo-aléatoires, parce qu'ils peuvent être reproduits si on connaît la graine (seed).

Il y a des alternatives à `math/rand` pour plus d'exigences, comme `crypto/rand` pour de la cryptographie plus sécurisée. Cependant, ces nombres sont générés différemment et sont difficilement prédictibles.

En ayant une graine basée sur l'heure (`time.Now().UnixNano()`), on introduit de l'incertitude car cette valeur change constamment. Sans graine, votre générateur produirait toujours la même séquence de nombres aléatoires. 

## See Also
Envie d'approfondir le sujet ? Jetez un œil à ces liens :

- Documentation officielle de Go sur `math/rand`: https://golang.org/pkg/math/rand/
- La notion de graine (seed) en génération de nombres aléatoires: https://en.wikipedia.org/wiki/Random_seed
- Une discussion approfondie sur les générateurs pseudo-aléatoires: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- `crypto/rand` pour la cryptographie: https://golang.org/pkg/crypto/rand/
