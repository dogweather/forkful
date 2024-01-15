---
title:                "Téléchargement d'une page web"
html_title:           "Javascript: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

De nos jours, la plupart d'entre nous passent une grande partie de notre temps en ligne, que ce soit pour travailler, étudier ou se divertir. Lorsque nous visitons un site web, nous téléchargeons en réalité une page web sur notre navigateur afin de pouvoir la visualiser et interagir avec elle. La compréhension du processus de téléchargement d'une page web peut donc être utile pour comprendre comment fonctionne internet et comment nous pouvons interagir avec les différents sites que nous visitons.

## Comment Faire

Pour télécharger une page web en utilisant Javascript, nous pouvons utiliser la méthode `fetch()`. Cette méthode effectue une requête HTTP afin de récupérer la réponse du serveur web et nous permet ainsi de télécharger la page web en utilisant le contenu de cette réponse.

Par exemple, si nous voulons télécharger la page d'accueil de Wikipedia, nous pouvons utiliser le code suivant dans notre navigateur :

```
Javascript
fetch('https://fr.wikipedia.org/')
.then(response => response.text())
.then(text => console.log(text));
```

Cela va effectuer une requête HTTP à https://fr.wikipedia.org/ et stocker la réponse dans la variable `text`. Nous pouvons ensuite afficher le contenu de cette variable dans la console de notre navigateur en utilisant la méthode `console.log()`.

## Plongée Profonde

La méthode `fetch()` utilise une technique appelée "promesses" (promises) pour gérer les requêtes asynchrones. Cela signifie que la méthode renvoie une promesse qui sera résolue plus tard avec le contenu de la réponse. Dans notre exemple, nous utilisons la méthode `then()` pour définir une fonction à exécuter une fois que la promesse est résolue. Dans ce cas, nous utilisons la fonction fléchée `=>` pour spécifier que nous voulons imprimer le contenu de la réponse dans la console.

En utilisant la méthode `fetch()`, nous pouvons également spécifier différents paramètres pour la requête, tels que la méthode HTTP à utiliser, les en-têtes de la requête, les données à envoyer, etc. De plus, la méthode `fetch()` nous permet de gérer les erreurs en utilisant la méthode `catch()`, qui exécute une fonction en cas d'échec de la promesse.

## Voir Aussi

- [Documentation officielle sur la méthode `fetch()` en javascript](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API)
- [Tutoriel sur l'utilisation de `fetch()` pour télécharger une page web en javascript](https://www.taniarascia.com/how-to-use-the-javascript-fetch-api-to-get-data/)
- [Article expliquant le concept de promesses en javascript](https://www.freecodecamp.org/news/a-simple-guide-to-understanding-javascript-promises-222e834bfabf/)