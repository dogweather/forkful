---
date: 2024-01-20 18:01:57.632330-07:00
description: 'Comment faire : Sample output.'
lastmod: '2024-04-05T21:53:59.677321-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
weight: 45
---

## Comment faire :
```Javascript
const axios = require('axios').default;
const base64 = require('base-64');

// Encodez votre login et mot de passe en Base64
const username = 'votre_login';
const password = 'votre_mot_de_passe';
const basicAuth = 'Basic ' + base64.encode(username + ':' + password);

// Créez et envoyez la requête HTTP avec Axios
axios.get('http://monapi.com/data', { headers: { 'Authorization': basicAuth } })
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.error('Erreur d’authentification:', error);
  });
```
Sample output:
```
{ "id": 1, "nom": "Exemple Data" }
```

## Deep Dive
Historiquement, l'authentification de base était une méthode simple pour sécuriser l'accès aux pages web. Aujourd'hui, des méthodes plus sécurisées comme OAuth 2 ou JWT sont souvent préférées, mais l'authentification de base reste pertinente pour des scripts ou des API internes. Techniquement, c'est simplement une chaîne 'username:password' encodée en Base64 dans l'en-tête `Authorization` d'une requête HTTP. Attention, sans HTTPS, les identifiants pourraient être interceptés !

## Voir Aussi
- [Axios Docs](https://axios-http.com/docs/intro)
- [Base-64 npm package](https://www.npmjs.com/package/base-64)
- [MDN Web Docs - Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [OAuth 2.0](https://oauth.net/2/)
- [JSON Web Tokens (JWT)](https://jwt.io/)
