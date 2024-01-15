---
title:                "Envoi d'une requête http avec authentification de base"
html_title:           "Gleam: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi
Tout d'abord, pourquoi quelqu'un voudrait-il envoyer une requête HTTP avec une authentification de base ? Eh bien, cette méthode est couramment utilisée pour accéder à des APIs sécurisées en fournissant un nom d'utilisateur et un mot de passe, permettant à l'utilisateur d'authentifier son identité auprès du serveur.

## Comment faire
Tout d'abord, il faut installer le module HTTP de Gleam en utilisant la commande `gleam install gleam/http`. Ensuite, nous pouvons construire notre requête en utilisant la fonction `build_with_basic_auth` de ce module, en passant les paramètres suivants : l'URL de la requête, le nom d'utilisateur et le mot de passe. Voici un exemple de requête GET en utilisant l'API de GitHub :

```Gleam
let request =
  Http.build_with_basic_auth(
   "https://api.github.com/users/octocat",
   "username",
   "password"
 )
  |> Http.get
```

Nous pouvons maintenant envoyer cette requête en utilisant la fonction `execute` du module HTTP, qui renvoie un tuple comprenant le code de statut de la réponse et son corps :

```Gleam
let response =
  request
    |> Http.execute
```

Et pour finir, nous pouvons traiter le corps de la réponse en utilisant les différentes fonctions du module `Json.Decode` pour décoder les données JSON retournées. Par exemple, pour accéder à la valeur de `"login"` dans notre réponse, nous pouvons utiliser la fonction `field` :

```Gleam
let login =
  response
    |> Json.Decode.decode_string
    |> Result.and_then(identity)
    |> Json.Decode.field("login", Json.Decode.string)
```

## Plongée en profondeur
Maintenant que nous avons vu comment envoyer une requête HTTP avec une authentification de base en utilisant Gleam, il est important de comprendre comment fonctionne réellement ce processus. Lorsque nous appelons la fonction `execute`, notre requête est transformée en un objet HTTP et envoyée au serveur. Si l'authentification est acceptée, le serveur renverra une réponse avec un code de statut 200, indiquant que la requête a été réussie. Sinon, un code d'erreur sera retourné avec un message indiquant que l'authentification a échoué.

## Voir aussi
- [Documentation du module HTTP de Gleam](https://gleam.run/modules/http.html)
- [Exemple de requête HTTP avec Gleam](https://github.com/gleam-lang/example-http-request)
- [Gleam sur GitHub](https://github.com/gleam-lang/gleam)