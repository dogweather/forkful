---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Générer des nombres aléatoires implique de produire une suite de nombres qui ne semblent suivre aucun schéma prévisible. C'est crucial pour de nombreux programmes, notamment pour les jeux, la cryptographie et les simulations.

## Comment faire :

Voici comment générer un nombre aléatoire dans Gleam:
```Gleam
import gleam/list.{shuffle} 
import gleam/map.new
import gleam/random.{generator, int, Range}

fn rand() {
  let manual_seed_generator = generator(1)
  let (manual_seed_generator, _seed) = int(Range(1, 100), manual_seed_generator)
  manual_seed_generator
}
```

Cet exemple réalise un générateur manuel puis génère un nombre aléatoire entre 1 et 100.

## Exploration Approfondie

Historiquement, le problème de la génération aléatoire a été abordé via diverses méthodes allant de l'irrégularité physique, comme le lancer d'un dé, à des algorithmes électriques et numériques. Gleam utilise un générateur de nombres aléatoires basé sur un mélange de mutabilité et de pureté fonctionnelle.

Dans Gleam, on pourrait penser à utiliser des alternatives comme `random.float` ou `random.bits` selon le type de données nécessaire. Toutes ces fonctions prennent un générateur de nombres aléatoires comme argument et retournent un nouveau générateur avec le résultat. Cela permet de maintenir une implication de pureté fonctionnelle.

## Voir Aussi

Pour un aperçu détaillé de la génération de nombres aléatoires dans Gleam, consultez la documentation officielle [ici](https://gleam.run/documentation/generating-random-numbers/).

Vous pouvez également consulter ce [guide](https://gleam.run/guide-to-random/) pour une exploration plus en profondeur sur la génération aléatoire dans Gleam.