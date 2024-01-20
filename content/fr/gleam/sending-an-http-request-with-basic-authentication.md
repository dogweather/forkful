---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?
L'envoi d'une requête HTTP avec authentification de base consiste à utiliser un nom d'utilisateur et un mot de passe pour accéder à certaines ressources sur le Web. Les programmeurs le font pour sécuriser l'accès à des ressources et services spécifiques.

## Comment faire :
Envoyer une requête HTTP avec une authentification de base en Gleam ressemble à cela :

```gleam
import gleam/httpc
import gleam/http.{Uri}

let request = httpc.new("GET", Uri.from_string("http://example.fr/").unwrap(), [], nil)
|> httpc.auth_basic("username", "password")

httpc.send(request)
```

La réponse pourrait ressembler à cela :

```gleam
Ok(#{
  body: "Ok",
  status: 200,
})
```

## Approfondissement
Historiquement, l'authentification de base est l'une des premières méthodes d'authentification adoptées dans les protocoles HTTP. Cependant, elle est souvent déconseillée dans les applications modernes en raison de problèmes de sécurité, sauf si elle est utilisée avec HTTPS.

Par ailleurs, l'authentification de base n'est pas la seule option pour sécuriser les requêtes HTTP. D'autres alternatives incluent l'authentification par jeton, l'authentification par hachage, etc.

En ce qui concerne Gleam, cette méthode effectue une requête en utilisant une interface haut niveau basée sur l'envoi d'une structure `Request`, qui est ensuite convertie en une requête HTTP bas niveau.

## Voir de plus
- Lisez la [RFC 7617](https://tools.ietf.org/html/rfc7617) pour comprendre les détails de l'authentification de base.
- Voici une [alternative à l'authentification de base](https://auth0.com/docs/tokens) pour une approche plus sécurisée.