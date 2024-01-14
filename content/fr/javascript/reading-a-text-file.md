---
title:                "Javascript: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Les fichiers texte sont l'un des types de fichiers les plus couramment utilisés en développement de logiciels. Ils contiennent souvent des données importantes et précieuses pour les applications. Apprendre à lire un fichier texte en utilisant du code Javascript est une compétence essentielle que tout développeur devrait posséder.

## Comment faire

Pour lire un fichier texte en Javascript, nous allons utiliser la fonction `fs.readFile` du module `fs` intégré de Node.js. Tout d'abord, nous avons besoin d'importer le module `fs` en l'assignant à une variable :

```Javascript
const fs = require('fs');
```

Ensuite, nous pouvons utiliser la fonction `fs.readFile` en lui passant le chemin du fichier ainsi que l'encodage de caractères (UTF-8) comme arguments :

```Javascript
fs.readFile('fichier.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data); // affiche le contenu du fichier
});
```

La fonction `readFile` prend également une fonction de retour d'appel (callback) en troisième paramètre, qui sera exécutée une fois que le fichier a été complètement lu. Nous utilisons `console.log` pour afficher le contenu du fichier sur la console. Vous pouvez également utiliser `data` pour manipuler les données du fichier, comme les stocker dans une variable ou les utiliser pour effectuer d'autres opérations.

## Plongée profonde

Il est important de noter que la fonction `fs.readFile` lit le fichier de manière asynchrone, ce qui signifie que l'exécution du code se poursuivra sans attendre que le fichier soit complètement lu. Si vous avez besoin de lire un fichier de manière synchrone, vous pouvez utiliser la fonction `fs.readFileSync` qui bloque l'exécution jusqu'à ce que le fichier soit complètement lu. Cependant, il est généralement déconseillé d'utiliser cette méthode, car elle peut ralentir les performances de votre application.

De plus, si vous devez lire des fichiers volumineux, il est préférable d'utiliser la fonction `createReadStream` qui lit le fichier par petits morceaux, ce qui est plus efficace en termes de mémoire.

## Voir aussi

- [Documentation de la fonction fs.readFile de Node.js](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_fs_readfile_path_options_callback)
- [Guide de Node.js sur la lecture de fichiers](https://nodejs.dev/learn/reading-files-with-nodejs)
- [Guide complet sur la manipulation des fichiers avec Node.js](https://www.digitalocean.com/community/tutorials/how-to-manipulate-files-in-node-js)