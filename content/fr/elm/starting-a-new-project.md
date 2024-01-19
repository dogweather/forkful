---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## C'est quoi & Pourquoi? 

Démarrer un nouveau projet en Elm, c'est initier une architecture d'application propre, structurée et de haute fiabilité. Les programmeurs le font pour créer des applications web frontales rapides, expérimentales et maintenables.

## Comment faire? 

Voici un exemple de comment commencer votre nouveau projet en utilisant `elm init`.

```Elm
# Créez un nouveau répertoire pour votre projet
$ mkdir MonProjetElm
$ cd MonProjetElm

# Lancez Elm Init pour initialiser le Projet
$ elm init

# Résultat
Elm projects always start with an elm.json file. I can create them!

Alright, it is all set up. I created an elm.json file with some reasonable defaults.

```

Après avoir lancé `elm init`, une structure de dossiers sera créée avec un fichier `elm.json`. 

## Plongée profonde 

Elm est un langage de programmation fonctionnel qui a été conçu pour la facilité d'emploi, la performance et la robustesse. Il a été introduit en 2012 par Evan Czaplicki et a reçu un accueil largement positif pour sa simplicité et sa productivité.

Pour démarrer un nouveau projet, on pourrait également utiliser `create-elm-app`, une alternative créée par la communauté qui initie un projet avec une structure plus riche, incluant le support pour le développement avec hot-reloading et le build de l'application en production.

Quand vous commencez un nouveau projet en Elm, le piliers sont le fichier `elm.json` qui contient la configuration de votre projet, et le répertoire `src` qui contient vos fichiers Elm. 

## Voir aussi 

- Documentation Elm : [lien](https://elm-lang.org/docs)
- Create Elm App : [lien](https://github.com/halfzebra/create-elm-app)
- Guide de démarrage rapide d'Elm : [lien](https://guide.elm-lang.org/install/elm.html)