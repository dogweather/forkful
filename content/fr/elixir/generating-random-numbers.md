---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:48:56.656547-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Générer des nombres aléatoires, c'est comme lancer un dé virtuel. On le fait pour introduire de l'imprévisibilité dans nos applications, pour des jeux, des simulations ou des choix aléatoires.

## How to:
En Elixir, utiliser `:rand.uniform/1` pour sortir un nombre aléatoire.

```elixir
# Générer un nombre aléatoire entre 1 et 10
random_number = :rand.uniform(10)
IO.puts(random_number)
```

Output pourrait être `7`, ou un autre chiffre entre 1 et 10.

Pour les graines aléatoires:

```elixir
# Initialiser la graine
:rand.seed(:exs1024, :os.timestamp())

# Maintenant générer un nombre
random_number = :rand.uniform(10)
IO.puts(random_number)
```

Le résultat change à chaque exécution.

## Deep Dive
Avant Elixir 1.3, `:random` était utilisé, mais a été remplacé par `:rand`. Pourquoi? Consistance et performance. `:rand` offre une meilleure uniformité sur les distributions de nombres.

Le seeding est crucial; il détermine la séquence de nombres aléatoires. Sans seeding explicite, `:rand` utilise un seed automatiquement généré. Mais avec seeding, comme avec `:os.timestamp()`, la séquence est plus prévisible en redémarrages.

Si `:rand` ne suffit pas, des librairies tierces comme `:exsplus` fournissent plus de fonctionnalités.

## See Also
- [Elixir School - Random](https://elixirschool.com/en/lessons/basics/enum#shuffle)
- [Erlang -- rand](http://erlang.org/doc/man/rand.html)