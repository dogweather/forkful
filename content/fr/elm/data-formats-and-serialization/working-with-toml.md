---
date: 2024-01-26 04:21:09.614536-07:00
description: "Comment faire : Elm n'a pas de parser TOML int\xE9gr\xE9, mais vous\
  \ pouvez interagir avec JavaScript ou utiliser un package de la communaut\xE9. Voici\
  \ comment vous\u2026"
lastmod: '2024-03-13T22:44:57.715440-06:00'
model: gpt-4-0125-preview
summary: "Elm n'a pas de parser TOML int\xE9gr\xE9, mais vous pouvez interagir avec\
  \ JavaScript ou utiliser un package de la communaut\xE9."
title: Travailler avec TOML
weight: 39
---

## Comment faire :
Elm n'a pas de parser TOML intégré, mais vous pouvez interagir avec JavaScript ou utiliser un package de la communauté. Voici comment vous pourriez analyser TOML en utilisant un package hypothétique `elm-toml` :

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

Pour décoder des valeurs spécifiques :

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

Un exemple de sortie pour `port` pourrait être `Ok 8080` si le décodage réussit.

## Plongée Profonde
TOML a été créé par Tom Preston-Werner, co-fondateur de GitHub, comme un langage simple pour les fichiers de configuration. Il est en compétition avec YAML et JSON ; la syntaxe de TOML vise le meilleur des deux mondes avec un accent sur la facilité de lecture et d'écriture par les humains.

Dans Elm, pour manipuler TOML, vous devez généralement passer par l'interopérabilité avec JavaScript, ce qui peut être un peu compliqué. Heureusement, la communauté Elm est ingénieuse, et plusieurs packages tiers existent. Le package hypothétique `elm-toml` utiliserait probablement le `Port` d’Elm pour communiquer avec un parser TOML JavaScript ou implémenterait le parsing directement en Elm.

Le principal obstacle dans Elm est qu'il tape tout statiquement, donc vous aurez besoin d'écrire des decodeurs personnalisés pour gérer différentes structures de données au sein de TOML, ce qui peut être un peu verbeux mais ajoute de la sécurité.

## Voir Aussi
Pour les spécifications et plus d'informations sur TOML lui-même, consultez [TOML](https://toml.io).
Si vous cherchez une approche pratique de l'interopérabilité Elm et JavaScript, commencez par le guide officiel : [Elm Ports](https://guide.elm-lang.org/interop/ports.html).
Pour les packages de la communauté ou pour contribuer, parcourez [Elm Packages](https://package.elm-lang.org/).
