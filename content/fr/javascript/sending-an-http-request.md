---
title:                "Envoyer une requête http"
html_title:           "Javascript: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il est important d'envoyer des requêtes HTTP en Javascript. Eh bien, cela peut être nécessaire pour récupérer des données à partir d'un serveur distant ou pour interagir avec des API.

## Comment faire

Voici un exemple de code pour envoyer une requête GET avec l'API fetch en Javascript :

```Javascript
fetch('https://example.com/api/users')
  .then(response => response.json())
  .then(data => {
    console.log(data); // Output: {users:[{name: "John", age: 30}, {name: "Jane", age: 28}]}
  });
```

Vous pouvez également spécifier les options de la requête, telles que la méthode, les en-têtes et le corps, comme ceci :

```Javascript
fetch('https://example.com/api/users', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  body: JSON.stringify({name: "Bob", age: 35})
})
.then(response => console.log(response.status)) // Output: 201 (created)
```

## Plongeons plus en profondeur

Lorsque vous envoyez une requête HTTP en Javascript, vous utilisez une interface appelée XMLHttpRequest (XHR). Cette interface vous permet d'envoyer des requêtes de manière asynchrone et de gérer les réponses. Vous pouvez également utiliser des bibliothèques telles que Axios pour simplifier le processus d'envoi de requêtes HTTP.

## Voir aussi

- [Documentation sur l'API fetch en Javascript](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API)
- [Tutoriel complet sur l'envoi de requêtes HTTP en Javascript](https://www.datacamp.com/community/tutorials/working-with-apis-using-javascript)