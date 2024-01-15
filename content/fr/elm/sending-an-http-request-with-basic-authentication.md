---
title:                "Envoi d'une demande http avec une authentification de base"
html_title:           "Elm: Envoi d'une demande http avec une authentification de base"
simple_title:         "Envoi d'une demande http avec une authentification de base"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous connaissez sûrement le sentiment de devoir entrer des identifiants à chaque fois que vous souhaitez accéder à une API ou un site web qui nécessite une authentification de base. Cela peut devenir fastidieux et prendre du temps, surtout si vous devez le faire fréquemment. Heureusement, en utilisant Elm, il est possible d'automatiser ce processus en envoyant une requête HTTP avec une authentification de base.

## Comment Faire

Pour commencer, assurez-vous d'avoir la dernière version d'Elm installée sur votre ordinateur. Ensuite, suivez ces étapes simples pour envoyer une requête HTTP avec une authentification de base :

1. Tout d'abord, importez le module `Http` dans votre fichier Elm.
2. Ensuite, créez une fonction `authenticate` qui prendra en paramètres l'URL de l'API ou du site web, ainsi que les identifiants d'authentification.
3. Utilisez la fonction `Http.basicAuth` pour créer un objet d'authentification en utilisant les identifiants fournis.
4. Utilisez la fonction `Http.send` pour envoyer une requête GET à l'URL avec l'objet d'authentification en tant que paramètre.
5. Gérez la réponse de la requête en utilisant un `case` statement et en accédant aux données renvoyées par l'API ou le site web.

Voici un exemple de code Elm qui envoie une requête GET à l'API https://example.com avec une authentification de base :

```Elm
import Http

authenticate : String -> String -> Http.Request a
authenticate url credentials =
    let
        auth =
            Http.basicAuth "username" "password"

        request =
            Http.get url auth
    in
        Http.send handleResponse request

handleResponse : Http.Response a -> Http.Error -> Platform.Cmd msg
handleResponse response error =
    case response of
        Http.BadUrl _ ->
            -- gestion d'erreur pour une mauvaise URL

        Http.Timeout ->
            -- gestion d'erreur pour une requête expirée ou un délai dépassé

        Http.NetworkError ->
            -- gestion d'erreur pour une erreur de connexion réseau

        Http.BadStatus _ ->
            -- gestion d'erreur pour un code d'état HTTP inattendu

        Http.GoodStatus ->
            -- traitement des données renvoyées par l'API ou le site web
```

Vous remarquerez que le code utilise un `case` statement pour gérer différentes erreurs qui pourraient survenir lors de l'envoi de la requête. Cela permet de s'assurer que votre application n'échoue pas en cas d'erreur.

## Plongée en Profondeur

Maintenant que vous savez comment envoyer une requête HTTP avec une authentification de base en utilisant Elm, il est important de comprendre le fonctionnement de cette méthode d'authentification. En utilisant l'objet d'authentification créé avec la fonction `Http.basicAuth`, Elm ajoute un en-tête `Authorization` à la requête HTTP avec les identifiants encodés en base64. Ce processus assure que les informations d'authentification ne sont pas envoyées en clair, ce qui est important pour la sécurité de vos données.

Il est également possible d'utiliser d'autres méthodes d'authentification en utilisant le module `Http` d'Elm. Par exemple, la fonction `Http.oauth` peut être utilisée pour envoyer une requête OAuth à une API.

## Voir Aussi

- Documentation officielle d'Elm sur l'utilisation du module `Http` : https://package.elm-lang.org/packages/elm/http/latest/
- Tutoriel sur la gestion des requêtes HTTP en utilisant Elm : https://www.elm-tutorial.org/fr/api/http-requests.html
- Exemple de code pour une authentification OAuth en utilisant Elm : https://github.com/danyx23/elm-oauth2-sample