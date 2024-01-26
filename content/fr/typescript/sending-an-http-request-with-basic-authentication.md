---
title:                "Envoi d'une requête HTTP avec authentification de base"
date:                  2024-01-20T18:02:47.319162-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Envoyer une requête HTTP avec une authentification de base signifie inclure des informations d'identification codées dans les en-têtes pour accéder à une ressource protégée. Les programmeurs utilisent cette technique lorsqu'ils ont besoin d'une méthode simple et rapide pour sécuriser les communications entre un client et un serveur.

## Comment faire :

```TypeScript
import axios from 'axios';

// Encoder les informations d'identification en base64
const username = 'votre_utilisateur';
const password = 'votre_motdepasse';
const basicAuth = 'Basic ' + Buffer.from(username + ':' + password).toString('base64');

// Configurer les en-têtes de la requête avec l'authentification de base
const config = {
  headers: {
    'Authorization': basicAuth
  }
};

// Envoyer la requête GET avec l'authentification de base
axios.get('https://votre-api.com/ressource', config)
  .then(response => {
    console.log('Response data:', response.data);
  })
  .catch(error => {
    console.error('Error:', error);
  });
```
*Cet exemple utilise Axios, une bibliothèque de client HTTP populaire.*

## Plongée Profonde

Historiquement, l'authentification de base HTTP était l'une des premières méthodes pour sécuriser les requêtes HTTP, mais elle est relativement floue car les identifiants sont simplement encodés en base64 sans cryptage. De nos jours, des alternatives plus sophistiquées comme OAuth ou JWT (Json Web Token) sont souvent préférées car elles offrent un meilleur niveau de sécurité. Néanmoins, pour des scénarios internes ou lorsque la facilité d'usage est essentielle, l'authentification de base reste une option viable. La clé de l'implémentation en TypeScript est l'encodage des crédentiels et leur ajout correct dans les en-têtes HTTP.

## Voir Aussi

- Documentation Axios sur GitHub : [https://github.com/axios/axios](https://github.com/axios/axios)
- Authentification HTTP de base sur MDN : [https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)
- Authentification JWT pour les applications modernes : [https://jwt.io/introduction/](https://jwt.io/introduction/)
