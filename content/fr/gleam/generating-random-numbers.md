---
title:                "Génération de nombres aléatoires"
date:                  2024-01-27T20:33:22.961600-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Générer des nombres aléatoires en programmation peut être crucial pour créer des simulations, des tests, de la cryptographie et des jeux. Dans Gleam, c'est une fonctionnalité qui permet aux développeurs d'introduire de l'imprévisibilité ou de simuler des scénarios du monde réel dans leurs applications.

## Comment faire :

Pour générer des nombres aléatoires dans Gleam, vous utilisez principalement la bibliothèque `gleam_random`. Cette bibliothèque fournit des fonctions pour générer des entiers aléatoires, des flottants, et plus encore. Premièrement, assurez-vous d'avoir ajouté `gleam_random` à votre fichier `rebar.config` ou `mix.exs` en tant que dépendance.

Plongeons dans quelques exemples :

### Générer un Entier Aléatoire

Pour produire un entier aléatoire dans une plage spécifiée, vous pouvez utiliser la fonction `int` :

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Cette fonction générera un entier aléatoire entre 1 et 10 inclus.

### Générer un Flottant Aléatoire

Pour obtenir un flottant aléatoire, utilisez la fonction `float`. Cela génère un flottant entre 0.0 et 1.0 :

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Exemple de Résultat

L'exécution de ces fonctions pourrait donner des résultats tels que :

- Pour `generate_random_int()`: `5`
- Pour `generate_random_float()`: `0.84372`

Rappelez-vous, chaque exécution pourrait conduire à des résultats différents en raison de la nature de l'aléatoire.

## Plongée Profonde

Le module `gleam_random` implémente un générateur de nombres pseudo-aléatoires (Pseudo Random Number Generator, PRNG), ce qui signifie essentiellement que les nombres ne sont pas vraiment aléatoires mais sont difficiles à prédire, en imitant l'aléatoire. Les PRNG fonctionnent en commençant avec une valeur initiale, connue sous le nom de graine, et en appliquant des opérations mathématiques pour générer une séquence de nombres.

Historiquement, les langages et bibliothèques ont implémenté plusieurs algorithmes pour les PRNG, comme le Mersenne Twister ou le générateur linéaire congruentiel (LCG). Le choix de l'algorithme impacte la qualité de l'"aléatoire", certains étant plus adaptés pour des applications cryptographiques que d'autres. Alors que la bibliothèque standard de Gleam offre commodité et facilité d'usage avec son module `gleam_random`, il peut ne pas toujours être le meilleur choix pour des cas d'usage nécessitant de l'aléatoire cryptographiquement sûr. Pour des fins cryptographiques, les développeurs devraient se tourner vers des bibliothèques spécifiquement conçues pour fournir des générateurs de nombres pseudo-aléatoires cryptographiquement sûrs (CSPRNGs), qui sont conçus pour résister aux attaques pouvant prédire les futurs nombres en observant une séquence de nombres générés.

En conclusion, bien que la fonctionnalité de génération de nombres aléatoires de Gleam soit robuste pour les besoins généraux de programmation, les applications ayant des exigences de sécurité spécifiques devraient envisager des solutions cryptographiques dédiées pour assurer l'intégrité et la sécurité de leur génération de nombres aléatoires.
