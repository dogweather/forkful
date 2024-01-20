---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'envoi d'une requête HTTP est un moyen pour votre application de communiquer avec un serveur web. C'est fondamental pour interagir avec des services web, récupérer des données, envoyer des données, et plus encore.

## Comment faire :

Pour envoyer une requête HTTP en Javascript, nous utiliserons la `fetch API`. Voici un exemple simple :

```Javascript
fetch('https://api.example.com/data', {
  method: 'GET', 
})
.then(response => response.json())
.then(data => console.log(data))
.catch((error) => {
  console.error('Erreur:', error);
});
```

Dans cet exemple, nous envoyons une requête GET à `https://api.example.com/data` et nous affichons les données reçues dans la console.

## Plongée en profondeur

Historiquement, la méthode `XMLHttpRequest` était utilisée pour envoyer des requêtes HTTP en Javascript. Cependant, sa syntaxe peut être verbeuse et complexe, et la `Fetch API` est maintenant recommandée pour une utilisation plus moderne et plus claire.

En ce qui concerne les alternatives, `axios` est une bibliothèque populaire qui offre une API similaire à `fetch`, mais avec quelques fonctionnalités supplémentaires comme l'interception de requêtes et de réponses et la possibilité de ne pas avoir à convertir la réponse en JSON.

Le fonctionnement interne de l'envoi de requêtes HTTP peut être complexe, mais pour faire simple : votre application envoie une requête HTTP à un serveur, le serveur traite la requête et renvoie une réponse, que votre application peut ensuite traiter.

## Voir aussi

- [Fetch API sur MDN](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API)
- [Axios sur Github](https://github.com/axios/axios)
- [XMLHttpRequest sur W3Schools](https://www.w3schools.com/xml/ajax_xmlhttprequest_send.asp)