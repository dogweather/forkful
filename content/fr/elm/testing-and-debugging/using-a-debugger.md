---
date: 2024-01-26 03:49:32.324341-07:00
description: "Comment faire : Elm n'a pas de d\xE9bogueur int\xE9gr\xE9 au sens traditionnel,\
  \ comme le fait, par exemple, JavaScript avec les outils de d\xE9veloppement du\u2026"
lastmod: '2024-03-13T22:44:57.693906-06:00'
model: gpt-4-0125-preview
summary: "Elm n'a pas de d\xE9bogueur int\xE9gr\xE9 au sens traditionnel, comme le\
  \ fait, par exemple, JavaScript avec les outils de d\xE9veloppement du navigateur."
title: "Utilisation d'un d\xE9bogueur"
weight: 35
---

## Comment faire :
Elm n'a pas de débogueur intégré au sens traditionnel, comme le fait, par exemple, JavaScript avec les outils de développement du navigateur. Cependant, la communauté Elm a développé des outils pour combler cette lacune. Voici comment vous pouvez utiliser `elm-debug-transformer` pour déboguer votre application Elm :

```Elm
-- Installer elm-debug-transformer (paquet Node)

1. npm install -g elm-debug-transformer

-- Utiliser elm-debug-transformer pour démarrer votre application

2. elm-debug-transformer --port=8000 votreFichierPrincipalElm.elm
```

Une fois `elm-debug-transformer` en cours d'exécution, il crée une connexion WebSocket pour la journalisation. Vous verrez les informations de débogage dans la console de votre navigateur où vous pourrez inspecter les structures de données de votre programme à des points donnés de votre application.

Dans Elm 0.19 et ultérieur, les fonctions du module `Debug` comme `Debug.log` et `Debug.todo` peuvent vous aider à tracer des valeurs et à marquer délibérément les parties inachevées de votre code. Voici comment utiliser Debug.log :

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Incrementation" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Decrementation" { model | count = model.count - 1 }, Cmd.none )
```

Vous verrez les messages "Incrementation" ou "Decrementation" dans la console de votre navigateur avec le nouvel état du `model`.

## Approfondissement
Evan Czaplicki, l'auteur d'Elm, visait à créer un langage dans lequel les bogues courants seraient impossibles ou faciles à détecter. C'est pourquoi le noyau d'Elm n'inclut pas de fonctions de débogage traditionnelles. L'analyse statique d'Elm et l'inférence de type contribuent massivement à réduire les erreurs d'exécution, ce qui diminue le besoin d'un débogage d'exécution sophistiqué. Les alternatives historiques incluaient l'utilisation du maintenant déprécié `elm-reactor`, qui offrait du débogage de voyage dans le temps - une façon de rembobiner et de rejouer des actions dans votre application.

Aujourd'hui, des outils comme `elm-debug-transformer` et l'utilisation du module `Debug` d'Elm aident à combler le fossé. Bien que le module `Debug` soit destiné à être utilisé uniquement pendant le développement et devrait être supprimé avant les builds de production, c'est un outil inestimable pour identifier et journaliser les changements d'état.

Gardez à l'esprit que les techniques de débogage JavaScript traditionnelles, comme les points d'arrêt ou l'exécution pas à pas, ne sont pas directement applicables en Elm en raison de son architecture et de la gestion des mises à jour d'état par le runtime Elm. Elm vous encourage à structurer votre programme de manière à ce que le flux de données soit clair et suive des garanties strictes de types et d'immutabilité, minimisant les cas où le débogage est nécessaire.

## Voir Aussi
- Guide officiel d'Elm sur la gestion des exceptions d'exécution : https://guide.elm-lang.org/error_handling/
- Répertoire GitHub de `elm-debug-transformer` : https://github.com/kraklin/elm-debug-transformer
- Fil de discussion Elm abordant les stratégies de débogage : https://discourse.elm-lang.org/c/show-and-tell/debugging
- Documentation du module `Debug` d'Elm : https://package.elm-lang.org/packages/elm/core/latest/Debug
