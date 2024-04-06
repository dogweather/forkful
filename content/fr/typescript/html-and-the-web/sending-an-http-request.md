---
date: 2024-01-20 18:01:04.176146-07:00
description: "Comment faire : Les requ\xEAtes HTTP sont le fondement du web depuis\
  \ sa cr\xE9ation par Tim Berners-Lee. Initialement con\xE7u pour les documents,\
  \ le protocole\u2026"
lastmod: '2024-04-05T22:51:11.526126-06:00'
model: gpt-4-1106-preview
summary: "Les requ\xEAtes HTTP sont le fondement du web depuis sa cr\xE9ation par\
  \ Tim Berners-Lee."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

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
