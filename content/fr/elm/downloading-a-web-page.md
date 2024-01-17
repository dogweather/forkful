---
title:                "Le téléchargement d'une page web"
html_title:           "Elm: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que le téléchargement de page web et pourquoi les programmeurs le font-ils?

Le téléchargement de pages web est le processus de récupération du code HTML, CSS et JavaScript d'une page web pour l'afficher dans un navigateur. Les programmeurs le font pour créer des sites web et des applications web.

## Comment faire:

```
Elm.Http.get "https://www.example.com" 
        |> Task.toResult 
        |> Process.await 
        |> Result.map .response.body 
        |> Result.withDefault "Failed to retrieve webpage"
```

Cet exemple de code utilise le module «Http» d'Elm pour envoyer une requête à l'URL donnée et récupérer le corps de la réponse. Le résultat est ensuite transformé en chaîne de caractères ou en message indiquant une erreur si la requête a échoué.

## Plongez plus en profondeur:

Le téléchargement de pages web est un élément clé de la création de sites web et d'applications web. Les programmeurs peuvent également utiliser d'autres langages et outils pour le faire, tels que JavaScript et jQuery. En utilisant Elm, un langage de programmation fonctionnel, les programmeurs peuvent bénéficier d'une syntaxe plus propre et plus sûre pour effectuer cette tâche.

## Voir aussi:

- Documentation Elm sur le téléchargement de pages web: https://package.elm-lang.org/packages/elm/http/latest/Http#request
- Article sur l'importance du téléchargement de pages web dans la création de sites web: https://www.lifewire.com/what-is-web-scraping-3468358