---
title:    "Go: Génération de nombres aléatoires"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une pratique courante en programmation. Cela peut être utile pour simuler des données, créer des jeux ou sécuriser les mots de passe.

## Comment Faire

Pour générer des nombres aléatoires en Go, nous pouvons utiliser la bibliothèque `math/rand`. Tout d'abord, nous devons importer cette bibliothèque dans notre programme :

```Go
import "math/rand"
```

Ensuite, nous devons initialiser la source de nombres aléatoires, en utilisant par exemple le temps actuel comme base :

```Go
rand.Seed(time.Now().UnixNano())
```

Maintenant, nous pouvons utiliser différentes fonctions pour obtenir des nombres aléatoires en fonction de nos besoins :

- `rand.Intn(n)` pour générer un entier aléatoire entre 0 et n-1
- `rand.Int()` pour générer un entier aléatoire positif ou négatif
- `rand.Float32()` ou `rand.Float64()` pour générer un nombre à virgule flottante entre 0 et 1

Voici un exemple de code qui génère dix nombres aléatoires entre 1 et 100 :

```Go
for i := 0; i < 10; i++ {
    fmt.Println(rand.Intn(100) + 1)
}
```

Output :

```
10
63
87
39
55
96
30
62
43
8
```

## Plongée Profonde

La génération de nombres aléatoires peut sembler simple, mais elle est basée sur des algorithmes sophistiqués. En réalité, il n'est pas vraiment possible de générer un nombre totalement aléatoire, car les ordinateurs sont des machines déterministes. Cela signifie qu'ils suivent des instructions logiques précises et ne peuvent pas prendre de décisions aléatoires.

Ainsi, la bibliothèque `math/rand` utilise un générateur de nombres pseudo-aléatoires (PRNG) basé sur l'algorithme de congruence linéaire. Ce type de PRNG utilise une formule mathématique pour générer des séquences de nombres qui semblent aléatoires, mais qui sont en réalité déterminées par une graine initiale.

Pour cette raison, il est important de bien initialiser la source de nombres aléatoires en utilisant une valeur qui change constamment, comme le temps, afin d'obtenir des résultats plus aléatoires.

## Voir Aussi

- [Documentation officielle de `math/rand`](https://golang.org/pkg/math/rand/)
- [Article sur la génération de nombres aléatoires en Go](https://programming.guide/go/random-number-generation.html)