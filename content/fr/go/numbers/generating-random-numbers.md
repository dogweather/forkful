---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:29.914548-07:00
description: "Comment faire : En Go, les nombres al\xE9atoires sont g\xE9n\xE9r\xE9\
  s en utilisant le paquet `math/rand` pour les nombres pseudo-al\xE9atoires ou `crypto/rand`\
  \ pour les\u2026"
lastmod: '2024-03-13T22:44:57.128645-06:00'
model: gpt-4-0125-preview
summary: "En Go, les nombres al\xE9atoires sont g\xE9n\xE9r\xE9s en utilisant le paquet\
  \ `math/rand` pour les nombres pseudo-al\xE9atoires ou `crypto/rand` pour les nombres\
  \ pseudo-al\xE9atoires cryptographiquement s\xE9curis\xE9s."
title: "G\xE9n\xE9ration de nombres al\xE9atoires"
weight: 12
---

## Comment faire :
En Go, les nombres aléatoires sont générés en utilisant le paquet `math/rand` pour les nombres pseudo-aléatoires ou `crypto/rand` pour les nombres pseudo-aléatoires cryptographiquement sécurisés. Explorons les deux.

### Utiliser `math/rand` pour les nombres pseudo-aléatoires
D'abord, importez le paquet `math/rand` et le paquet `time` pour initialiser le générateur. L'initialisation garantit que vous obtenez une séquence différente de nombres à chaque exécution.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Un nombre aléatoire:", rand.Intn(100)) // Génère un nombre entre 0 et 99
}
```

Exemple de sortie : `Un nombre aléatoire: 42`

### Utiliser `crypto/rand` pour des nombres pseudo-aléatoires cryptographiquement sécurisés
Pour les applications plus sensibles à la sécurité, le paquet `crypto/rand` convient car il génère des nombres aléatoires difficiles à prédire, les rendant adaptés aux opérations cryptographiques.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Un nombre aléatoire sécurisé:", n)
}
```

Exemple de sortie : `Un nombre aléatoire sécurisé: 81`

## Approfondissement
La différence fondamentale entre les paquets `math/rand` et `crypto/rand` en Go découle de leur source d'entropie et de leurs cas d'utilisation prévus. `math/rand` génère des nombres pseudo-aléatoires basés sur une graine initiale ; ainsi, la séquence est déterministe et peut être prédite si la graine est connue. Cela convient aux scénarios où la performance élevée et non l'imprévisibilité absolue est la préoccupation clé, comme les simulations ou les jeux.

D'autre part, `crypto/rand` tire l'aléatoire du système d'exploitation sous-jacent, le rendant adapté aux usages cryptographiques où l'imprévisibilité est cruciale. Cependant, cela se fait au détriment de la performance et de la complexité dans la manipulation des nombres qu'il génère (comme traiter avec le type `*big.Int` pour les entiers).

Historiquement, la notion de génération de nombres aléatoires dans les ordinateurs a toujours dansé sur le fil de la vraie "aléatoire", avec des systèmes précoces dépendant fortement d'algorithmes déterministes qui imitaient l'aléatoire. À mesure que les ordinateurs évoluaient, ces algorithmes se sont également développés, incorporant des sources d'entropie plus sophistiquées de leur environnement.

Malgré ces avancées, la quête de la perfection aléatoire dans l'informatique est intrinsèquement paradoxale, étant donné la nature déterministe des ordinateurs eux-mêmes. C'est pourquoi, pour la plupart des applications où la prévisibilité serait préjudiciable, les nombres pseudo-aléatoires cryptographiquement sécurisés provenant de sources comme `crypto/rand` sont la meilleure alternative, malgré leur surcharge.

En essence, l'approche de Go avec deux paquets distincts pour la génération de nombres aléatoires aborde élégamment les compromis entre performance et sécurité, permettant aux développeurs de choisir en fonction de leurs besoins spécifiques.
