---
title:                "TypeScript: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi télécharger une page web ? Eh bien, que vous soyez un développeur web expérimenté ou un débutant curieux, comprendre comment télécharger une page web est une compétence essentielle pour créer des applications web dynamiques et interactives. Dans cet article, nous allons plonger dans les bases de la programmation TypeScript pour télécharger une page web en utilisant un exemple concret.

## Comment faire

Pour commencer, vous aurez besoin d'un éditeur de code et d'un navigateur web. Si vous êtes nouveau dans le monde de la programmation, vous pouvez utiliser un éditeur de code gratuit et open-source tel que Visual Studio Code et un navigateur populaire comme Google Chrome.

Maintenant, pour télécharger une page web en utilisant TypeScript, nous allons utiliser la bibliothèque native de Node.js appelée "https". Tout d'abord, créons un nouveau projet TypeScript en utilisant la commande suivante :

```TypeScript
$ npm init -y
$ npm install typescript --save-dev
```

Ensuite, créons un fichier "index.ts" et ajoutons le code suivant :

```TypeScript
import * as https from 'https';

const url = 'https://example.com';

https.get(url, (response) => {
  let data = '';

  response.on('data', (chunk) => {
    data += chunk;
  });

  response.on('end', () => {
    console.log(data);
  });

}).on("error", (err) => {
  console.log("Erreur : " + err.message);
});
```

Ce code importe la bibliothèque "https" et utilise sa méthode "get" pour télécharger la page web à partir de l'URL spécifiée. Ensuite, il stocke les données téléchargées dans une variable et les affiche dans la console une fois le téléchargement terminé.

Pour exécuter le code, nous devons le compiler en utilisant la commande suivante :

```TypeScript
$ tsc index.ts
```

Cela va créer un fichier JavaScript que nous pouvons exécuter en utilisant Node.js en saisissant la commande suivante :

```TypeScript
$ node index.js
```

Si tout se passe bien, vous devriez voir le contenu de la page web téléchargée dans la console !

## Plongée en profondeur

Maintenant que vous avez compris comment télécharger une page web en utilisant TypeScript, vous voudrez peut-être explorer davantage cette fonctionnalité en modifiant le code et en ajoutant des fonctionnalités telles que la gestion des erreurs ou la création d'un fichier local contenant les données téléchargées.

Vous pouvez également explorer la bibliothèque "https" pour découvrir toutes les fonctionnalités qu'elle offre, telles que l'envoi de demandes POST et la validation des certificats.

## Voir aussi

Pour en savoir plus sur TypeScript et les différentes fonctionnalités qu'il offre, consultez ces ressources utiles :

- [Site officiel de TypeScript](https://www.typescriptlang.org/) - Documentation officielle et guides de démarrage rapide.
- [Node.js Documentation](https://nodejs.org/en/docs/) - Documentation complète sur Node.js et ses bibliothèques natives.
- [Visual Studio Code](https://code.visualstudio.com/) - Éditeur de code gratuit et open-source avec prise en charge intégrée de TypeScript.
- [Google Chrome Developer Tools](https://developers.google.com/web/tools/chrome-devtools) - Outils de développement de Chrome pour déboguer vos applications web.