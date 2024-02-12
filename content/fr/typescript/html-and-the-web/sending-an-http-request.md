---
title:                "Envoi d'une requête HTTP"
aliases:
- /fr/typescript/sending-an-http-request.md
date:                  2024-01-20T18:01:04.176146-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Envoyer une requête HTTP, c'est demander à un serveur web de vous fournir des données, ou d'y soumettre des infos. Les devs font ça pour intéragir avec des services web - récupérer des données, envoyer des formulaires, autant de fonctionnalités essentielles pour les applis modernes.

## Comment faire :
```TypeScript
import axios from 'axios';

// Envoi d'une requête GET simple
axios.get('https://api.exemple.com/data')
  .then(response => {
    console.log(response.data); // Gérer la réponse ici
  })
  .catch(error => {
    console.error("Erreur lors de la requête : ", error);
  });

// Envoi d'une requête POST avec quelques données
axios.post('https://api.exemple.com/submit', { nom: 'Doe', prénom: 'Jane' })
  .then(response => {
    console.log(response.data); // La réponse du serveur
  })
  .catch(error => {
    console.error("Erreur lors de la requête : ", error);
  });
```

Sortie attendue :
```
{ "id": 1, "message": "Données récupérées avec succès" }
{ "id": 2, "message": "Données soumises avec succès" }
```

## Exploration approfondie
Les requêtes HTTP sont le fondement du web depuis sa création par Tim Berners-Lee. Initialement conçu pour les documents, le protocole HTTP s'est adapté pour les APIs et les services web. Axios est un choix populaire mais il y a d'autres bibliothèques comme fetch (natif en JavaScript) et des frameworks côté serveur tels que Express.js qui simplifient la création de requêtes HTTP.

Axios est apprécié pour sa simplicité et sa configuration plus complet par rapport à `fetch`. Cependant, `fetch` est native, signifiant que vous n'avez pas à installer quoi que ce soit de plus pour l'utiliser dans un environnement de navigateur moderne. Côté serveur, dans Node.js, `axios` est toujours un excellent choix.

Quand vous construisez une requête, vous spécifiez les en-têtes, les paramètres d'URL, les méthodes (GET, POST, etc.), et peut-être le corps de la requête. En réponse, vous obtenez des données, des codes de statut, et d'autres en-têtes, qui vous indiquent comment le serveur a traité votre requête.

## À voir également
- Documentation d'Axios : [https://axios-http.com/docs/intro](https://axios-http.com/docs/intro)
- API Fetch pour les requêtes modernes côté client : [https://developer.mozilla.org/fr/docs/Web/API/Fetch_API](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API)
- Guide HTTP sur MDN : [https://developer.mozilla.org/fr/docs/Web/HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP)
