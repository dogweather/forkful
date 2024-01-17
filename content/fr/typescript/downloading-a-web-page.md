---
title:                "Télécharger une page web"
html_title:           "TypeScript: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?
Télécharger une page web signifie récupérer le contenu d'une page web à partir d'un serveur distant. Les programmeurs le font pour pouvoir accéder et manipuler ces données pour l'utiliser dans leurs projets.

## Comment faire:
```TypeScript
import axios from 'axios'; //Importer la bibliothèque axios qui facilite les requêtes HTTP

axios.get('https://example.com/') //Envoyer une requête GET à l'URL souhaitée
  .then(function (response) { //Traiter la réponse avec une fonction de rappel
    console.log(response.data); //Afficher le contenu de la page téléchargée
  })
  .catch(function (error) { //Gérer les erreurs avec une fonction de rappel
    console.log(error); 
  });
```
**Exemple de sortie :**
```
<!DOCTYPE html>
<html>
  <head>
    <title>Example Domain</title>
    <meta charset="utf-8" />
  </head>
  <body>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
  </body>
</html>
```

## Plongée en profondeur:
Télécharger une page web est un processus couramment utilisé en programmation, en particulier pour les applications web et les outils de scraping (récupération automatique de données). Il existe plusieurs alternatives pour télécharger une page web, telles que l'utilisation de bibliothèques telles que Request ou Puppeteer, ou la mise en place d'un serveur proxy pour récupérer les données. L'implémentation peut varier en fonction de la bibliothèque utilisée et des paramètres de la requête.

## Voir aussi:
- [Documentation de la bibliothèque Axios](https://github.com/axios/axios)
- [Utiliser Axios dans un projet Node.js](https://blog.abelotech.com/posts/axios-cancel-request/)
- [Alternatives pour télécharger une page web en TypeScript](https://stackoverflow.com/questions/42084910/download-html-source-code-with-typescript-node-js/42092276)