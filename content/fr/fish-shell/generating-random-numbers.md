---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La génération de nombres aléatoires consiste à créer une série de chiffres qui ne présente aucun schéma prévisible. Les développeurs l'utilisent souvent pour les jeux, les simulations, et l'ensemble du code où l'incertitude est nécessaire.

## Comment faire:

Dans Fish Shell, la génération d'un nombre aléatoire est aussi simple que `random`. Voici quelques exemples.

```Fish Shell
# Si vous voulez un nombre aléatoire entre 1 et 10
set randomNumber (random 1 10)
echo $randomNumber
```

```Fish Shell
# Pour un nombre aléatoire entre 100 et 200
set anotherRandom (random 100 200)
echo $anotherRandom
```

Cela produira une sortie comme celle-ci:
```
7
167
```

## Plongée profonde

Historiquement, la génération de nombres aléatoires dans les systèmes Unix est basée sur `/dev/random` ou `/dev/urandom`. Cependant, Fish Shell simplifie cette tâche avec la commande `random`.

Une alternative possible est d'utiliser `jot` dans certaines versions de Unix. Cependant, Fish Shell n'utilise pas `jot` pour la commande `random`.

Concernant les détails de mise en œuvre, Fish utilise une combinaison de l'horloge du système et du générateur congruent linéaire pour donner des nombres pseudo-aléatoires. Il convient de noter que ce n'est pas une méthode cryptographiquement sûre pour la génération de nombres aléatoires.

## Voir aussi

1. Documentation officielle de `random`: https://fishshell.com/docs/current/cmds/random.html
2. Discussion sur `jot` vs `random` dans Fish: https://github.com/fish-shell/fish-shell/issues/2394
3. How computers generate random numbers: https://www.howtogeek.com/183051/htg-explains-how-computers-generate-random-numbers/