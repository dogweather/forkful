---
date: 2024-01-19
description: "How to: Elm ne poss\xE8de pas de syst\xE8me de fichiers int\xE9gr\xE9\
  , car il est con\xE7u pour la s\xE9curit\xE9 dans des applications web. Pour \xE9\
  crire un fichier, on\u2026"
lastmod: '2024-03-13T22:44:57.709534-06:00'
model: unknown
summary: "Elm ne poss\xE8de pas de syst\xE8me de fichiers int\xE9gr\xE9, car il est\
  \ con\xE7u pour la s\xE9curit\xE9 dans des applications web."
title: "\xC9criture d'un fichier texte"
weight: 24
---

## How to:
Elm ne possède pas de système de fichiers intégré, car il est conçu pour la sécurité dans des applications web. Pour écrire un fichier, on interagit souvent avec une API backend ou on génère un téléchargement côté client. Voici un exemple de génération d'un fichier texte à télécharger via Elm :

```Elm
module Main exposing (..)

import Browser
import Html exposing (a, text)
import Html.Attributes exposing (href, download)

main =
    Html.beginnerProgram { model = model, view = view, update = update }

model = "Données à enregistrer dans le fichier texte."

update _ model = model

view model =
    let
        encodedData =
            "data:text/plain;charset=utf-8," ++ (encodeURIComponent model)
    in
    a [ href encodedData, download "Data.txt" ] [ text "Télécharger le fichier" ]

encodeURIComponent : String -> String
encodeURIComponent =
    Js.encodeURIComponent
```

Lorsque vous cliquez sur le lien, "Data.txt" est téléchargé avec le texte "Données à enregistrer dans le fichier texte."

## Deep Dive
Historiquement, Elm avait des moyens restreints de manipulation de fichiers côté client et aucun du côté serveur, car le langage vise la fiabilité et la sécurité dans les applications web. Les alternatives incluent l'utilisation de JavaScript via des Ports ou de WebAssembly pour la communication entre Elm et les systèmes de fichiers. Ces méthodes offrent plus de flexibilité mais exigent une attention à la sécurité.

## See Also
- Documentation Elm sur les Ports: https://guide.elm-lang.org/interop/ports.html
- Comment encoder des URI en JavaScript : https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
- Exemple d'utilisation de WebAssembly avec Elm: https://discourse.elm-lang.org/t/using-webassembly-with-elm/450
