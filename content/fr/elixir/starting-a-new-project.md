---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Commencer un nouveau projet en programmation signifie initier le développement d'une nouvelle idée ou solution. Les programmeurs font cela pour résoudre des problèmes, explorer de nouvelles technologies ou mettre en œuvre des concepts innovants.

## Comment faire :
Créons un nouveau projet Elixir avec Mix, l'outil de création de projets d'Elixir qui gère les dépendances et les tâches.

```Elixir
# Commencer un nouveau projet
mix new mon_projet
```

Naviguez dans le répertoire du projet, et vous verrez la structure du dossier comme suit:

```Elixir
_mon_projet
|_ config
|_ lib
  |_ mon_projet.ex
|_ test
|_ mix.exs
```

`mix.exs` nous donne des informations de configuration pour notre projet. 


## Deep Dive
1. **Contexte historique** : Mix, introduit avec Elixir, est inspiré par des outils similaires dans d'autres écosystèmes de langage tels que Ruby's Bundler. Il vise à simplifier la gestion de projet en Elixir.
2. **Alternatives** : En dehors de Mix, on pourrait utiliser des outils comme Rebar3 (popularisé par le langage Erlang) pour créer des projets Elixir. Cependant, Mix est généralement préféré pour son intégration fluide avec l'écosystème Elixir.
3. **Détails d'implémentation** : Mix crée une structure de projet standard en Elixir. Il gère également les dépendances, ce qui facilite l'ajout de bibliothèques à votre projet.

## Voir aussi 
- [Documentation officielle de Mix](https://hexdocs.pm/mix/Mix.html)
- [Guide "Getting started" avec Elixir](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)