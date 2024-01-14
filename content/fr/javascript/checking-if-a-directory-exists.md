---
title:                "Javascript: Vérifier si un répertoire existe"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi Vérifier si un Répertoire Existe en Javascript

Vérifier si un répertoire existe est une étape importante dans le développement en Javascript. Cela peut être utile lors de la manipulation de fichiers et de dossiers, en s'assurant de l'existence d'un chemin avant de le parcourir ou de le lire. Cela peut également être utile pour garantir la sécurité de l'application et éviter des erreurs potentielles en cas de manipulation de données sensibles.

## Comment Vérifier si un Répertoire Existe en Javascript

Il existe plusieurs façons de vérifier si un répertoire existe en Javascript, nous allons en explorer deux ici.

La première méthode consiste à utiliser la fonction `fs.existsSync()` qui permet de vérifier l'existence d'un répertoire en utilisant le module `fs` de Node.js. Voici un exemple de code:

```Javascript
const fs = require('fs');
const folderPath = './exemple/dossier'; // chemin du dossier à vérifier

if (fs.existsSync(folderPath)) {
  // le dossier existe, on peut effectuer des actions sur celui-ci
  console.log("Le dossier existe !");
} else {
  // le dossier n'existe pas, on peut le créer ou afficher un message d'erreur
  console.log("Le dossier n'existe pas !");
}
```

La deuxième méthode utilise l'API Web `window.Directory` disponible dans les navigateurs modernes. Elle permet de vérifier l'existence d'un répertoire dans le navigateur sans avoir besoin de Node.js. Voici un exemple de code:

```Javascript
const folderPath = './exemple/dossier'; // chemin du dossier à vérifier

if (window.Directory) {
  const directory = new Directory();
  directory.getDirectory(folderPath, {}, (directoryEntry) => {
    // le dossier existe, on peut effectuer des actions sur celui-ci
    console.log("Le dossier existe !");
  }, (error) => {
    // le dossier n'existe pas, on peut le créer ou afficher un message d'erreur
    console.log("Le dossier n'existe pas !");
  });
}
```

## Plongée Profonde dans la Vérification d'Existence d'un Répertoire

Il est important de noter que les deux méthodes mentionnées ci-dessus ne garantissent pas que le chemin spécifié est bien un répertoire. Ils vérifient simplement si le nom du dossier existe dans le répertoire parent.

De plus, il est important de prendre en compte que la vérification de l'existence d'un répertoire peut être coûteuse en termes de performances, surtout si le chemin spécifié est un chemin réseau ou un chemin distant.

Pour ces raisons, il est recommandé d'utiliser la méthode `fs.stat()` plutôt que `fs.existsSync()` dans un environnement Node.js, car elle fournit plus d'informations sur le chemin, y compris s'il s'agit d'un fichier ou d'un répertoire.

## Voir Aussi

- [Documentation Node.js File System Module](https://nodejs.org/api/fs.html)
- [Documentation Web API Directory](https://developer.mozilla.org/fr/docs/Web/API/Directory)
- [Tutoriel sur la Manipulation de Fichiers en Javascript](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)