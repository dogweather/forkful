---
title:                "Génération de nombres aléatoires"
aliases:
- /fr/elixir/generating-random-numbers.md
date:                  2024-01-27T20:33:30.222539-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

La génération de nombres aléatoires en Elixir est une tâche de programmation fondamentale, essentielle pour les applications nécessitant des résultats imprévisibles tels que la génération de jetons sécurisés, l'échantillonnage de données ou dans les algorithmes de jeux. Les programmeurs l'utilisent pour introduire un niveau d'aléatoire et de variabilité dans leurs applications, les rendant plus dynamiques et moins déterministes.

## Comment faire :

Pour générer des nombres aléatoires en Elixir, vous utilisez principalement le module `:rand` qui fournit plusieurs fonctions à cet effet. Voici un guide rapide pour vous lancer :

D'abord, assurez-vous d'initialiser le générateur de nombres aléatoires avec un point de départ unique :

```elixir
:rand.seed(:exsplus)
```

Pour générer un entier aléatoire dans une plage, utilisez :

```elixir
random_integer = :rand.uniform(10) # Génère un nombre entre 1 et 10
IO.puts(random_integer)
```

Pour un flottant aléatoire entre 0 et 1,0 :

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

Vous pourriez avoir besoin d'une plage plus spécifique pour les flottants, ce qui nécessite un peu plus de calcul :

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

Rappelez-vous, ces nombres sont pseudo-aléatoires ; ils sont déterminés par la graine et l'algorithme mais suffisent pour la plupart des applications.

## Plongée Profonde

Les capacités de génération de nombres aléatoires d'Elixir reposent sur le module `:rand` d'Erlang, reflétant son héritage et sa relation étroite avec Erlang. Le module `:rand` a remplacé l'ancien module `:random`, offrant des algorithmes améliorés pour la génération de nombres aléatoires. Il fournit une variété d'algorithmes, le défaut étant `exsplus`, mais supporte également d'autres comme `exs64`, `exsl`, et plus, chacun ayant ses compromis en termes de vitesse et de qualité de l'aléatoire.

Un aspect intéressant de la génération de nombres aléatoires en Elixir (et donc en Erlang) est sa gestion des graines. Le système maintient des états de graine séparés pour chaque processus, assurant que des processus concurrents ne perturbent pas les séquences de nombres aléatoires des uns et des autres. Ceci est particulièrement utile dans les applications concurrentielles, assurant la prévisibilité et la fiabilité dans les systèmes distribués.

Alors que le module `:rand` suffit pour la plupart des cas d'utilisation, les applications nécessitant des nombres aléatoires cryptographiquement sécurisés devraient considérer d'autres options. Le module `crypto` fournit des fonctions comme `crypto:strong_rand_bytes/1` qui sont conçues pour générer des données aléatoires sécurisées adaptées à des fins cryptographiques. Ces alternatives sont essentielles pour les applications sensibles à la sécurité, comme la génération de jetons, le chiffrement et certains types de mécanismes d'authentification.
