---
title:                "Lancement d'un nouveau projet"
date:                  2024-01-20T18:03:39.562813-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lancement d'un nouveau projet"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Commençons un nouveau projet. Un besoin ou une idée, c'est souvent comme ça que ça commence. Nous, programmeurs, on crée des projets pour transformer ces étincelles en produits fonctionnels, testables, et évolutifs.

## Comment faire :

```gleam
// Installez Gleam si ce n'est pas déjà fait:
// curl https://sh.gleam.run | sh

// Créez un nouveau projet Gleam
$ gleam new mon_super_projet
$ cd mon_super_projet

// Examinez l'arborescence du projet
$ tree
.
├── gleam.toml
├── README.md
├── rebar.config
├── src
│   └── mon_super_projet.gleam
└── test
    └── mon_super_projet_test.gleam

// Votre monde de possibilités commence ici
```

## Plongée profonde

L'outil de création de nouvelle application, intégré à Gleam depuis ses débuts, s'inspire d'outils similaires de langages plus anciens comme Ruby's `rails new` ou Elixir's `mix new`. Ces commandes permettent de créer une structure standardisée pour débuter un développement avec une base cohérente.

L'alternative serait de tout faire à la main. Mais pourquoi réinventer la roue ? Avec Gleam, votre focus se porte sur le code, pas sur la mise en place de l'environnement.

Gleam est jeune (première release en 2019), mais il vise l’interopérabilité avec Erlang et Elixir, donnant accès à un écosystème riche. Il tire parti de la robuste machine virtuelle Erlang (BEAM), tout en ajoutant le typage statique pour plus de sécurité.

## Voir aussi

- Documentation de Gleam : [https://gleam.run/](https://gleam.run/)
- GitHub de Gleam pour des exemples avancés : [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)