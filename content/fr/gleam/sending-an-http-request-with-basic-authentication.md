---
title:                "Envoi d'une requête HTTP avec authentification de base"
date:                  2024-01-20T18:01:28.811377-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Envoyer une requête HTTP avec une authentification de base, c'est utiliser une méthode standard pour accéder à des ressources sécurisées sur le web. Les programmeurs l'utilisent pour s'assurer que seul les utilisateurs autorisés peuvent interagir avec certains services web.

## How to:
```gleam
import gleam/http
import gleam/http/elli
import gleam/string

pub fn basic_auth_header(username: String, password: String) -> http.Header {
  let credentials = string.concat([username, ":", password])
  let encoded_credentials = base64.encode(credentials)
  http.header("Authorization", "Basic " ++ encoded_credentials)
}

pub fn send_request() {
  let auth_header = basic_auth_header("user", "pass")
  let request = http.Request(
    method: http.Get,
    url: "https://example.com/protected",
    headers: [auth_header],
    ..http.default_request()
  )
  let response = elli.send(request)
  case response {
    Ok(response) -> 
      io.print(response.body) // Afficher le corps de la réponse
    Error(error) -> 
      io.print(error)         // Gestion des erreurs
  }
}

// Exemple de sortie
"The protected resource content."
```

## Deep Dive
L'authentification HTTP de base encode les identifiants `username:password` en Base64, puis les transmet via l'entête 'Authorization'. Historiquement, elle est simple mais peu sécurisée, car Base64 est facilement décodable. Aujourd'hui, on l'emploie moins souvent au profit de méthodes plus sûres comme l'authentification par jetons (token authentication) ou OAuth.

Dans Gleam, qui est un langage fonctionnel typé statiquement s'exécutant sur la BEAM (la machine virtuelle d'Erlang), la gestion de ces requêtes est efficace et exprimée de manière concise. Gleam a emprunté des concepts à Erlang, qui est robuste et éprouvé pour les systèmes distribués. Les avantages de Gleam incluent la sûreté de type et une syntaxe plaisante qui élabore sur les fondations d'Erlang.

## See Also
- Documentation Gleam sur HTTP - https://gleam.run/stdlib/gleam/http/
- Guide sur l'authentification HTTP - https://developer.mozilla.org/fr/docs/Web/HTTP/Headers/Authorization
- Base64 encoding - https://en.wikipedia.org/wiki/Base64