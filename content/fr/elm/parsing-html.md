---
title:                "Analyse de HTML"
html_title:           "Elm: Analyse de HTML"
simple_title:         "Analyse de HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur ou développeuse web, il est probable que vous ayez déjà dû manipuler des données HTML. Que ce soit pour créer des sites web ou pour extraire des informations d'une page web, la manipulation de HTML est inévitable. Dans cet article, nous allons plonger dans l'un des aspects fondamentaux du développement web: l'analyse (ou le parsing) de HTML en utilisant le langage de programmation Elm.

## Comment faire

L'analyse de HTML en Elm peut sembler intimidante, mais grâce à sa bibliothèque HTML, cela peut être fait de manière facile et efficace. Voici un exemple de code Elm pour extraire le contenu d'un titre HTML d'une page web:

```Elm
import Html exposing (..)

-- Crée un élément HTML à partir d'une chaîne de caractères
htmlFromString : String -> Html.Html
htmlFromString str =
  case Html.parse str of
    Ok tree ->
      tree

    Err error ->
      errorMsg error

-- Élément HTML pour représenter un titre
type alias Title = String

-- Parse le titre HTML
parseTitle : String -> Title
parseTitle html =
  case htmlFromString html |> Html.firstChild |> Html.firstChild of
    Title title ->
      title

    _ ->
      "Titre non trouvé"

-- Exemple d'utilisation
myTitle : Title
myTitle =
  parseTitle "<h1>Hello, World!</h1>" -- Output: "Hello, World!"
```

Comme on peut le voir dans cet exemple, l'utilisation de la bibliothèque HTML permet d'extraire facilement le contenu d'un élément HTML. Il suffit de fournir une chaîne de caractères représentant le code HTML et d'utiliser les fonctions de la bibliothèque pour accéder aux différentes parties de l'arborescence.

## Plongée en profondeur

Maintenant que nous avons vu un exemple simple de parsing de HTML en Elm, il est important de noter quelques points importants:

- La bibliothèque HTML d'Elm n'est pas conçue pour l'analyse complète de pages web, mais pour extraire des données spécifiques d'un élément HTML.
- La fonction `Html.parse` renvoie un type `Result` qui peut être soit `Ok` ou `Err`, il est donc important de gérer les cas d'erreur pour éviter des bugs.
- Il existe d'autres fonctions utiles dans la bibliothèque HTML pour accéder aux différents attributs et enfants d'un élément HTML, il est donc conseillé de les explorer pour une meilleure compréhension.

La manipulation de HTML en Elm peut sembler un peu complexe au début, mais une fois que vous maîtriserez la bibliothèque HTML, elle deviendra un outil précieux pour votre développement web.

## Voir aussi

- [Documentation officielle Elm HTML](https://package.elm-lang.org/packages/elm/html/latest/)
- [Article sur l'analyse de JSON en Elm](https://programmer.group/parsing-json-in-elm.html)