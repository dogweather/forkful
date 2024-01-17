---
title:                "Envoyer une requête http"
html_title:           "Elm: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Envoyer une demande HTTP est une action courante dans la programmation Elm. Il s'agit de faire une requête à un serveur pour récupérer des données ou effectuer une action. Les programmeurs utilisent les demandes HTTP pour communiquer avec des API, récupérer des informations à jour ou envoyer des données depuis leur application.

## Comment faire:

Voici un exemple de code montrant comment faire une demande HTTP en utilisant la bibliothèque "Http" intégrée à Elm:

```Elm
import Http

-- faire une demande GET à l'adresse "https://mon-serveur.com/api/articles"
Http.get "https://mon-serveur.com/api/articles"
    |> Http.send ArticlesRecus -- ArticlesRecus sera la fonction de gestion de la réponse

-- fonction de gestion de la réponse
ArticlesRecus : Http.Response a -> Msg
ArticlesRecus response =
    case response of
        -- si la demande a réussi, récupérer le contenu de la réponse
        Http.Ok articles ->
            let
                contenu = articles.body
            in
                -- faire quelque chose avec le contenu récupéré
                MsgTraitement contenu

        -- si la demande a échoué, renvoyer une erreur
        Http.Err erreur ->
            Error erreur

```

## Plongée en profondeur:

Les demandes HTTP ont été introduites pour la première fois en 1991 et ont depuis été largement utilisées pour communiquer entre les serveurs et les clients sur le web. Bien qu'elles soient toujours largement utilisées, il existe maintenant plusieurs alternatives telles que GraphQL ou gRPC qui offrent des fonctionnalités plus avancées pour les demandes de données.

L'implémentation d'une demande HTTP avec Elm est assez simple grâce à la bibliothèque intégrée. Cependant, il est important de considérer la gestion des erreurs et les bonnes pratiques pour optimiser les performances.

## Voir aussi:

Pour en savoir plus sur les demandes HTTP en Elm, consultez la documentation officielle: [https://package.elm-lang.org/packages/elm/http/latest/]

Vous pouvez également découvrir d'autres alternatives pour la récupération de données telles que GraphQL: [https://package.elm-lang.org/packages/dillonkearns/elm-graphql/latest/]