---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:04.773343-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Générer des nombres aléatoires, c'est créer des numéros imprévisibles et sans aucun modèle. Les programmeurs en ont besoin pour tout, de la sécurisation des données à la création de jeux, en passant par la simulation et les tests.

## Comment faire :

Récupérez un nombre aléatoire entre 0 et 100 :

```Fish Shell
set -l random_number (random 0 100)
echo $random_number
```

Sortie d'exemple :

```
42
```

Pour un nombre à l'intérieur d'un tableau :

```Fish Shell
set -l numbers (seq 1 10)
echo $numbers[(random 1 (count $numbers))]
```

Sortie d'exemple :

```
7
```

## Plongée profonde

Fish Shell, depuis la version 3.0.0, embarque une commande `random` pour générer des nombres aléatoires. Avant ça, on devait s'appuyer sur `$RANDOM` ou invoquer des commandes externes comme `shuf`. 

Les deux principales alternatives sont `jot` ou `awk` pour des systèmes sans `random`. Concernant les détails d'implémentation, Fish utilise `arc4random_uniform` ou des fonctionnalités similaires de la bibliothèque C pour assurer une distribution équitable des nombres.

## Voir Aussi

- Documentation officielle de Fish Shell pour `random`: https://fishshell.com/docs/current/cmds/random.html
- Article sur les générateurs de nombres aléatoires (en anglais): https://en.wikipedia.org/wiki/Random_number_generation
- Comparatif des méthodes pour générer des nombres aléatoires dans différents shells : https://unix.stackexchange.com/questions/140750/enerate-random-numbers-in-specific-range
