---
title:                "Envoi d'une requête http avec authentification de base"
html_title:           "TypeScript: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

L'envoi de requêtes HTTP avec une authentification de base est une pratique courante dans le développement d'applications web. Cela permet de sécuriser l'accès à certaines données ou fonctionnalités en vérifiant l'identité de l'utilisateur.

## Comment faire

```typescript
// Importer le module "http" de Node.js
import * as http from 'http';

// Définir les informations d'authentification
const username = 'utilisateur';
const password = 'motdepasse';

// Créer l'en-tête d'authentification de base en encodant en base64 les identifiants
const authHeader = 'Basic ' + Buffer.from(username + ':' + password).toString('base64');

// Définir les options de la requête
const options = {
  hostname: 'www.exemple.com',
  path: '/api/endpoint',
  method: 'GET',
  headers: {
    'Authorization': authHeader // Ajouter l'en-tête d'authentification
  }
};

// Envoyer la requête
const req = http.request(options, (res) => {
  console.log(`Status: ${res.statusCode}`); // Afficher le code de statut de la réponse
  console.log('Headers: ', res.headers); // Afficher les en-têtes de la réponse
  res.on('data', (data) => {
    console.log('Body: ', data); // Afficher le corps de la réponse
  })
});

// Traitement des erreurs
req.on('error', (error) => {
  console.error(error); 
});

// Terminer la requête
req.end();
```

Dans l'exemple ci-dessus, nous utilisons le module "http" de Node.js pour envoyer une requête HTTP avec une authentification de base. Nous définissons d'abord les informations d'authentification, puis nous créons l'en-tête d'authentification en utilisant l'encodage Base64. Ensuite, nous définissons les options de la requête, en ajoutant l'en-tête d'authentification. Enfin, nous envoyons la requête en utilisant la méthode "http.request()" et traitons la réponse et les éventuelles erreurs.

## Deep Dive

L'authentification de base est la méthode la plus simple pour sécuriser l'accès à une ressource HTTP. Elle utilise un encodage en Base64 pour transmettre le nom d'utilisateur et le mot de passe dans l'en-tête "Authorization" de la requête. Cependant, cette méthode n'est pas sécurisée car les identifiants sont facilement déchiffrables et peuvent être interceptés par un tiers.

Pour une sécurité renforcée, il est recommandé d'utiliser d'autres méthodes d'authentification telles que l'authentification par jeton (token) ou à l'aide d'un certificat.

## Voir aussi

- [Documentation Node.js - module "http"](https://nodejs.org/docs/latest-v16.x/api/http.html)
- [Guide MDN - Authentication - Basic_Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Package "request" pour simplifier les requêtes HTTP en Node.js](https://www.npmjs.com/package/request)