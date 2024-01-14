---
title:                "Javascript: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

L'envoi d'une requête HTTP fait partie intégrante de la programmation web. Cela permet aux développeurs d'envoyer et de recevoir des données entre un client et un serveur, créant ainsi une communication bidirectionnelle. Cela permet également aux applications web de fournir des informations dynamiques et de fournir une expérience utilisateur fluide.

## Comment faire

Pour envoyer une requête HTTP en Javascript, vous pouvez utiliser la méthode `fetch ()`. Cela vous permet d'envoyer une requête à une URL spécifique et de récupérer la réponse sous forme de promesse. Voyons un exemple de code ci-dessous:

```
fetch("https://reqres.in/api/users/1")
  .then(response => response.json())
  .then(user => console.log(user))
```

Dans cet exemple, nous envoyons une requête à l'API ReqRes qui nous renvoie les informations de l'utilisateur avec un ID de 1. Nous utilisons ensuite la méthode `json ()` pour extraire les données de réponse au format JSON et les afficher dans la console. Le résultat de cet exemple sera:

```
{ "data": { "id": 1, "email": "george.bluth@reqres.in", "first_name": "George", "last_name": "Bluth", "avatar": "https://reqres.in/img/faces/1-image.jpg" } }
```

## Plongée en profondeur

Il est important de noter que lors de l'envoi d'une requête HTTP en Javascript, il existe différents types de méthodes que vous pouvez utiliser en fonction de ce que vous souhaitez réaliser. Les plus couramment utilisées sont `GET`, `POST`, `PUT` et `DELETE`.

- `GET` est utilisé pour obtenir des informations à partir d'une URL spécifique.
- `POST` est utilisé pour envoyer des données à une URL spécifique pour créer ou mettre à jour une ressource.
- `PUT` est utilisé pour mettre à jour une ressource existante à une URL spécifique.
- `DELETE` est utilisé pour supprimer une ressource à une URL spécifique.

Il est également possible d'ajouter des paramètres et des en-têtes à votre requête HTTP en utilisant des options de configuration supplémentaires.

## Voir aussi

- [Documentation sur la méthode fetch ()](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API/Using_Fetch)
- [Guide sur les méthodes HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Methods)
- [API ReqRes](https://reqres.in)