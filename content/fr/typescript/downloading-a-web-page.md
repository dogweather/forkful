---
title:                "Téléchargement d'une page web"
html_title:           "TypeScript: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Télécharger une page web peut sembler une tâche simple, mais c'est en fait un processus complexe qui implique de nombreuses étapes. En comprenant comment cela fonctionne, vous pourriez être en mesure de résoudre des problèmes de téléchargement et d'améliorer votre expérience en ligne.

## Comment faire

```TypeScript
import * as request from 'request';

// URL de la page à télécharger
const url = "https://www.example.com";

// Utilisation de la bibliothèque request pour effectuer une requête GET
request(url, function(error, response, body) {
  // Vérification d'erreurs
  if (error) {
    console.log(error);
  }
  // Vérification du code de réponse HTTP
  if (response.statusCode === 200) {
    // Enregistrement du contenu de la page dans un fichier
    fs.writeFile("page.html", body, function(err) {
      if (err) {
        console.log(err);
      }
      console.log("Page téléchargée avec succès !");
    });
  } else {
    console.log("La page n'a pas été téléchargée. Code de réponse : " + response.statusCode);
  }
});
```

Le code ci-dessus utilise la bibliothèque externe "request" pour effectuer une requête GET vers l'URL spécifiée et enregistrer le contenu de la page dans un fichier HTML.

## Plongée en profondeur

Bien qu'il existe plusieurs bibliothèques et outils pour télécharger une page web en TypeScript, la plupart d'entre eux suivent un processus similaire : effectuer une requête HTTP vers l'URL, extraire le contenu de la réponse et le traiter selon les besoins. Il est important de comprendre que la structure d'une page web peut varier considérablement et nécessiter des manipulations de données spécifiques pour être correctement téléchargée.

## Voir aussi

- [Utilisation de la bibliothèque Request](https://www.npmjs.com/package/request)
- [Guide de débogage des problèmes de téléchargement](https://www.webpagefx.com/web-design/debugging-download-problems.html)
- [Référence de TypeScript pour les requêtes HTTP](https://www.typescriptlang.org/docs/handbook/integrating-with-build-tools.html#making-requests)