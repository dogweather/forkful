---
date: 2024-01-26 04:13:13.861339-07:00
description: "La boucle Lire-\xC9valuer-Imprimer (REPL) est un environnement de programmation\
  \ simple et interactif qui prend en charge les entr\xE9es utilisateur\u2026"
lastmod: '2024-03-13T22:44:57.690831-06:00'
model: gpt-4-0125-preview
summary: "La boucle Lire-\xC9valuer-Imprimer (REPL) est un environnement de programmation\
  \ simple et interactif qui prend en charge les entr\xE9es utilisateur individuelles,\
  \ les \xE9value, et renvoie le r\xE9sultat \xE0 l'utilisateur."
title: Utilisation d'une console interactive (REPL)
weight: 34
---

## Comment faire :
Elm ne vient pas avec un REPL intégré. Cependant, vous pouvez utiliser `elm repl` depuis votre ligne de commande pour démarrer une session Elm après avoir installé Elm.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

Dans cette session, après avoir importé les fonctions List, nous avons doublé les nombres dans une liste et obtenu le résultat instantanément.

## Plongée profonde
Le REPL d'Elm peut sembler limité par rapport à ceux de certains autres langages comme Python ou JavaScript, car Elm est un langage compilé axé sur la production d'applications web. Historiquement, Elm s'est concentré sur des applications complètes plutôt que sur des scripts ou des interactions en ligne de commande.

Les alternatives au REPL d'Elm comprennent `elm-live` et des éditeurs en ligne comme Ellie où vous pouvez voir les modifications du code reflétées en temps réel dans un navigateur.

Concernant l'implémentation, le REPL d'Elm compile des extraits de code Elm en JavaScript en arrière-plan, vous permettant d'exécuter Elm de manière interactive. C'est différent des REPL des langues interprétées, qui n'ont pas besoin de cette étape de compilation. Le REPL d'Elm est également simplifié pour garder le langage central léger et concentré.

## Voir aussi
- Le guide officiel d'Elm sur l'interactivité : https://guide.elm-lang.org/interop/
- Ellie, un terrain de jeu Elm en ligne : https://ellie-app.com/new
- `elm-live`, un serveur de développement flexible pour Elm : https://www.elm-live.com/
