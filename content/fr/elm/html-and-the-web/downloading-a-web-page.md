---
date: 2024-01-20 17:43:54.263147-07:00
description: "How to: (Comment faire :) En Elm, on utilise le package `Http` pour\
  \ les requ\xEAtes web. Voil\xE0 un exemple basique ."
lastmod: '2024-04-05T21:53:59.183670-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) En Elm, on utilise le package `Http` pour les requ\xEA\
  tes web."
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to: (Comment faire :)
En Elm, on utilise le package `Http` pour les requêtes web. Voilà un exemple basique :

```Elm
import Http
import Json.Decode as Decode

type Msg = GotPageContent (Result Http.Error String)

getPage : Cmd Msg
getPage =
    Http.get
        { url = "https://example.com"
        , expect = Http.expectString GotPageContent
        }

init : ( Model, Cmd Msg )
init =
    ( initialModel, getPage )

-- N'oubliez pas de gérer GotPageContent dans votre update.
```

Et voici le résultat :

```Elm
GotPageContent (Ok "<html>...</html>") -- en cas de succès
GotPageContent (Err ...) -- en cas d'erreur
```

## Deep Dive (Plongée en profondeur)
Historiquement, le téléchargement de pages web en Elm s'est amélioré avec des abstractions sûres pour éviter les erreurs à l'exécution. Les alternatives comme les `WebSockets` sont utilisées pour des données en temps réel, mais sont plus complexes. Pour notre `Http.get`, Elm utilise des `Decoders` pour traiter les données de retour, assurant que les résultats soient conformes à nos attentes.

## See Also (Voir aussi)
- Documentation Elm `Http`: https://package.elm-lang.org/packages/elm/http/latest/
- Guide Elm sur les requêtes HTTP: https://guide.elm-lang.org/effects/http.html
- Json.Decode documentation: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode
