---
title:                "Envoi d'une requête HTTP"
date:                  2024-01-20T17:59:58.342925-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Envoyer une requête HTTP, c'est demander quelque chose à un autre serveur web. On le fait pour récupérer des données, soumettre des formulaires ou intéragir avec des services web.

## How to:
En JavaScript moderne, on utilise `fetch` pour les requêtes HTTP. C'est simple et promis-centric.

```javascript
// GET Request.
fetch('https://api.example.com/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Une erreur est survenue:', error));

// POST Request.
fetch('https://api.example.com/data', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({ key: 'value' }),
})
.then(response => response.json())
.then(data => console.log(data))
.catch(error => console.error('Une erreur est survenue:', error));
```

Sortie attendue : Affiche les données récupérées en console.

## Deep Dive
Avant, `XMLHttpRequest` était le standard pour les requêtes HTTP. Mais c'était compliqué et moins élégant. `fetch` est arrivé avec les promesses, simplifiant le code. Attention, `fetch` ne rejette pas les promesses pour des réponses HTTP à statut d'erreur (comme 404 ou 500). Il faut vérifier `response.ok`.

Pour de vieux navigateurs, il faut des polyfills ou revenir à `XMLHttpRequest`. Avec Node.js, on utilise des modules comme `axios` ou le récent `node-fetch`.

## See Also
- MDN Web Docs sur "Fetch" : [fetch() - MDN](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API/Using_Fetch)
- Polyfill `fetch` pour la compatibilité : [GitHub - whatwg-fetch](https://github.com/github/fetch)
- Node.js `node-fetch` : [node-fetch - npm](https://www.npmjs.com/package/node-fetch)
