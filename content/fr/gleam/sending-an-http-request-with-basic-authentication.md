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

## Quoi & Pourquoi?
Envoyer une requête HTTP avec une authentification de base est une méthode couramment utilisée par les programmeurs pour accéder à des ressources en ligne protégées par un nom d'utilisateur et un mot de passe. Cette méthode permet de sécuriser l'accès aux ressources pour empêcher tout accès non autorisé.

## Comment Faire:
Pour envoyer une requête HTTP avec une authentification de base en utilisant Gleam, vous pouvez suivre ces étapes simples:

1. Importez le module `http` dans votre code Gleam:
```Gleam
import http
```

2. Définissez les informations d'authentification de base dans une structure `Http.BasicAuth`:
```Gleam
let auth = {
  username: "utilisateur",
  password: "mot_de_passe"
}
```

3. Utilisez la fonction `http.request()` pour envoyer la requête en spécifiant les informations d'authentification:
```Gleam
http.request(
  method = .get
  url = "https://exemple.com/ressource/"
  auth = .basic("utilisateur", "mot_de_passe")
)
```

4. Vous pouvez également spécifier d'autres options de requête telles que l'en-tête `Content-Type` en utilisant la structure `Http.RequestOptions`:
```Gleam
http.request(
  method = .post
  url = "https://exemple.com/ressource/"
  auth = .basic("utilisateur", "mot_de_passe")
  options = {
    headers = dict.from_list([("Content-Type", "application/json")])
  }
  body = "{\"key\": \"valeur\"}"
)
```

## Deep Dive:
- La méthode d'authentification de base a été introduite pour la première fois dans la spécification HTTP 1.0 en 1996.
- Une alternative à l'authentification de base est l'authentification par token, où un jeton unique est généré pour chaque utilisateur pour accéder aux ressources protégées.
- Vous pouvez également implémenter manuellement l'envoi d'une requête HTTP avec authentification de base en utilisant un client HTTP tel que cURL.

## Voir Aussi:
- Documentation Gleam sur l'envoi de requêtes HTTP: https://gleam.run/modules/http.html
- Spécification HTTP 1.0: https://tools.ietf.org/html/rfc1945
- Guide cURL sur l'authentification de base: https://ec.haxx.se/http-auth.html