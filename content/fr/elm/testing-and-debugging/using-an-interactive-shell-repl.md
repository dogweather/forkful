---
title:                "Utilisation d'une console interactive (REPL)"
aliases:
- /fr/elm/using-an-interactive-shell-repl/
date:                  2024-01-26T04:13:13.861339-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La boucle Lire-Évaluer-Imprimer (REPL) est un environnement de programmation simple et interactif qui prend en charge les entrées utilisateur individuelles, les évalue, et renvoie le résultat à l'utilisateur. Les programmeurs Elm utilisent le REPL pour des expériences rapides, le débogage ou pour apprendre le langage.

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
