---
title:                "La lecture d'un fichier texte"
html_title:           "Javascript: La lecture d'un fichier texte"
simple_title:         "La lecture d'un fichier texte"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par la programmation, il est fort probable que vous ayez déjà eu besoin de lire un fichier texte dans votre code. Cela peut être utile pour différentes raisons, comme récupérer des données stockées dans un fichier ou encore pour créer un programme qui analyse un texte pour en extraire des informations.

## Comment Faire

Pour lire un fichier texte en Javascript, nous allons utiliser l'API File System (fs) qui fait partie du module natif d'Node.js. Voici un exemple de code pour lire un fichier texte et afficher son contenu dans la console :

```Javascript
const fs = require('fs');

fs.readFile('monFichier.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Dans cet exemple, nous utilisons la méthode `readFile` de l'API `fs` pour lire le fichier `monFichier.txt`. Le premier argument est le nom du fichier que nous voulons lire et le deuxième argument est l'encodage du fichier (ici, `utf8`). La méthode prend également une fonction de rappel en paramètre, qui sera exécutée une fois que le fichier aura été lu. Dans cette fonction, nous vérifions d'abord s'il y a une erreur avec `err`, et si c'est le cas, nous la lançons avec l'instruction `throw`. Sinon, nous affichons le contenu du fichier dans la console avec `console.log(data)`.

## Plongée en Profondeur

Il est important de noter que la méthode `readFile` est asynchrone, ce qui signifie qu'elle ne bloquera pas l’exécution du reste du code. Cela est utile lorsque nous avons besoin de continuer à exécuter d'autres tâches pendant que le fichier est en train d'être lu. Nous pouvons également utiliser la méthode `readFileSync` pour lire le fichier de manière synchrone, mais cela bloquera l'exécution du code jusqu'à ce que le fichier soit entièrement lu.

De plus, la méthode `readFile` renvoie un objet `Buffer` si aucun encodage n'est spécifié. Pour lire le contenu du fichier en tant que chaîne de caractères, il est donc nécessaire de spécifier l'encodage comme dans l'exemple précédent.

Il est également possible de lire un fichier ligne par ligne en utilisant la méthode `createReadStream` et en écoutant l'événement `line` de l'objet retourné. Ce peut être utile pour des fichiers de grande taille où il n'est pas nécessaire de lire le fichier entier en une seule fois.

## Voir Aussi

- Documentation officielle de l'API File System: https://nodejs.org/api/fs.html
- Tutoriel sur la lecture de fichiers en Node.js: https://www.tutorialsteacher.com/nodejs/nodejs-file-system
- Exemple pratique: Analyse de texte avec Node.js: https://www.digitalocean.com/community/tutorials/how-to-use-node-js-and-the-filesystem-to-serve-html-files