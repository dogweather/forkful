---
title:                "Envoi d'une requête HTTP"
date:                  2024-01-20T17:59:37.675304-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Envoyer une requête HTTP, c'est demander à un serveur web de vous renvoyer des données. Les programmeurs envoient des requêtes HTTP pour récupérer des informations ou interagir avec des services web.

## Comment faire :

```Gleam
import gleam/http
import gleam/httpc

pub fn send_request() {
    let response = 
        httpc.send(http.Request(
            method: http.Get,
            url: "https://jsonplaceholder.typicode.com/posts/1",
            headers: [],
            body: http.Empty,
        ))

    case response {
        Ok(response) -> 
            io.println("Voici votre réponse: \(response.body)")
        Error(e) ->
            io.println("Oups ! Il y a eu une erreur : \(e)")
    }
}
```

Sortie échantillon :
```
Voici votre réponse: {"userId": 1, "id": 1, "title": "sunt aut facere repellat provident ..."}
```

## Deep Dive

Historiquement, HTTP (HyperText Transfer Protocol) était pour les pages web simples. Aujourd'hui, son utilité s'étend aux services web interactifs. En Gleam, `httpc` est le module pour envoyer des requêtes HTTP, un peu comme `requests` en Python. D'autres options incluent des clients HTTP asynchrones ou des outils comme `curl` en ligne de commande. En interne, `httpc` se charge de préparer la requête, de la sécuriser si nécessaire, et de gérer la réponse du serveur.

## Voir Également

- Documentation de `httpc` : https://hexdocs.pm/gleam_httpc/gleam/httpc/
- Guide pour commencer avec Gleam : https://gleam.run/book/tour/
- HTTP sur MDN pour comprendre les bases du protocole HTTP : https://developer.mozilla.org/en-US/docs/Web/HTTP