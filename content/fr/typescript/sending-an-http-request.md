---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-Ce Que C'est & Pourquoi?

Envoyer une requête HTTP, c'est demander à un serveur de vous fournir des données ou d'effectuer une action. En tant que programmeur, vous faites cela pour interagir avec des applications Web, récupérer, modifier, ajouter ou supprimer des données.

## Comment faire:

Voici comment vous pouvez envoyer une requête HTTP avec le module `http` dans TypeScript:

```TypeScript
import * as http from 'http';

const options = {
  hostname: 'www.example.com',
  port: 80,
  path: '/api/data',
  method: 'GET'
};

const req = http.request(options, response => {
  let data = '';

  response.on('data', chunk => {
    data += chunk;
  });

  response.on('end', () => {
    console.log(data);
  });
});

req.on('error', error => {
  console.error(error);
});

req.end();
```

Ceci envoie une requête GET à `www.example.com/api/data` et affiche la réponse dans la console.

## Plongée Profonde:

L'envoi de requêtes HTTP est fondamental pour le Web depuis les années 90. C'était, et reste encore, la principale méthode d'échange de données sur Internet. Des alternatives telles que WebSockets et GraphQL offrent des paradigmes d'interaction différents, mais les requêtes HTTP restent le moyen le plus populaire pour la communication client-serveur.

Dans TypeScript, il existe plusieurs façons d'envoyer des requêtes HTTP. Vous pouvez utiliser le module `http` incorporé comme montré ci-dessus, ou vous pouvez utiliser des bibliothèques tierces comme `axios` ou `fetch`. Chacune a ses propres avantages et inconvénients, mais elles suivent toutes le même modèle de base: créer une requête, l'envoyer et gérer la réponse.

## Voir Aussi:

* [MDN Web Docs](https://developer.mozilla.org/fr/docs/Web/HTTP) - Un guide complet sur les requêtes HTTP.
* [Fetch API](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API) - Une alternative moderne pour faire des requêtes HTTP dans le navigateur.
* [Axios](https://axios-http.com/) - Une bibliothèque populaire pour faire des requêtes HTTP en JavaScript / TypeScript.