---
title:                "Téléchargement d'une page web"
html_title:           "Elm: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en herbe ou un expert chevronné, il est toujours excitant de créer une application web fonctionnelle de A à Z. Mais avant de pouvoir lancer votre application, il est crucial de savoir comment télécharger une page web.

## Comment faire

Pour télécharger une page web en Elm, vous pouvez utiliser la fonction `Http.get` et spécifier l'URL de la page en tant que paramètre. Voici un exemple de code pour télécharger la page "https://mon-site-web.com":

```Elm
import Html exposing (text)
import Http

main =
  Html.text "Téléchargement en cours..."

-- Exécution de la fonction Http.get avec l'URL en paramètre
Http.get "https://mon-site-web.com" -- Remplacez par l'URL de votre choix
  |> Task.map .body -- Récupération du contenu de la page téléchargée
  |> Task.toMaybe -- Transformation en type Maybe pour gérer les possibles erreurs
  |> Task.andThen -- Opération de chaînage pour gérer les éventuelles erreurs
    (\response ->
      case response of
        -- Si la réponse est valide, afficher le contenu de la page
        Ok body ->
          Html.text body

        -- Si la réponse est une erreur, afficher un message d'erreur
        Err err ->
          Html.text ("Erreur lors du téléchargement : " ++ err)
    )
```

En exécutant ce code, vous devriez voir le contenu de la page téléchargée s'afficher dans votre navigateur.

## Plongée en profondeur

Pour les plus curieux, il est également possible de personnaliser les requêtes HTTP en utilisant la fonction `Http.request`. Cela vous permet de spécifier des en-têtes, des paramètres ou même des méthodes différentes de GET pour votre requête.

De plus, si vous avez besoin d'interagir avec des API externes, vous pouvez utiliser la bibliothèque de programmation asynchrone `elm/http` qui fournit des fonctions utiles pour effectuer des requêtes HTTP de manière plus simple.

## Voir aussi

- Documentation officielle sur `Http.get` : https://package.elm-lang.org/packages/elm/http/latest/Http#get
- Documentation officielle sur `Http.request` : https://package.elm-lang.org/packages/elm/http/latest/Http#request
- Documentation officielle sur la bibliothèque `elm/http` : https://package.elm-lang.org/packages/elm/http/latest/