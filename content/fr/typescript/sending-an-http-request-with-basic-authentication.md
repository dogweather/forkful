---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'envoi d'une requête HTTP avec authentification de base est un moyen pour une application web de valider l'identité d'un utilisateur. Généralement, c'est fait en passant des identifiants d'utilisateur par le header de la requête. C'est crucial pour sécuriser les données sensibles des utilisateurs.

## Comment faire:
Voici un exemple rapide de la façon dont vous pouvez réaliser cela en TypeScript en utilisant l'API fetch.

```TypeScript
import fetch from 'node-fetch';

const url = 'https://your-api-url.com';
const headers = {
    'Authorization': 'Basic ' + Buffer.from('username:password').toString('base64')
}

fetch(url, { headers })
    .then(response => response.json())
    .then(data => console.log(data))
    .catch(error => console.log('Erreur:', error));
```
Lorsqu'on commence, on obtient des données brutes émulées.

## Plongeon profond
Historiquement, l'authentification de base était parmi les premières méthodes pour valider les utilisateurs sur le web. C'est encore largement utilisé, malgré l'existence d'autres options plus sécurisées comme l'authentification à deux facteurs.

Des alternatives à l'authentification de base incluent l'authentification de porteur de token, l'authentification OAuth, et l'authentification Digest. Chaque option a ses avantages et inconvénients, alors il est important de choisir le meilleur pour votre application.

Concernant les détails d'implémentation, l'authentification de base est simple, n'exigeant que l'ajout d'un header 'Authorization' à votre requête HTTP. En TypeScript, on peut utiliser l'API intégrée fetch qui fait cela facilement.

## Voir aussi
Pour des informations plus détaillées sur l'implémentation de l'authentification de base en TypeScript, vous pouvez consulter ces articles :
- [MDN - Basic authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Typescript - Fetch API Usage](https://www.typescriptlang.org/docs/handbook/2/classes.html#instance-methods)
- [Node.js - HTTP Request with Basic Authentication](https://nodejs.org/api/http.html#http_http_request_options_callback)