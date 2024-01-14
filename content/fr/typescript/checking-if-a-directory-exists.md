---
title:    "TypeScript: Vérification de l'existence d'un répertoire"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi il est important de vérifier si un répertoire existe

Il est essentiel de vérifier si un répertoire existe avant d'effectuer des opérations de lecture ou d'écriture sur celui-ci. Cela permet de s'assurer que le programme ne rencontrera pas d'erreurs et fonctionnera de manière fluide en cas d'absence de ce répertoire.

## Comment faire

Dans TypeScript, il existe une fonction prédéfinie appelée ```existsSync()``` qui permet de vérifier si un répertoire existe ou non. Voici un exemple de code:

```
const fs = require('fs');

if (fs.existsSync('/chemin/vers/mon/repertoire')) {
  console.log("Le répertoire existe!");                  
} else {
  console.log("Le répertoire n'existe pas.");                  
}
```

Si le répertoire existe, le programme affichera "Le répertoire existe!" dans la console. Sinon, il affichera "Le répertoire n'existe pas.".

Il est également possible d'utiliser la fonction ```statSync()``` pour obtenir plus d'informations sur un répertoire spécifique, telles que sa taille, sa date de création, etc.

## Plongée en profondeur

Il est important de noter que la fonction ```existsSync()``` utilise une approche synchrone, ce qui signifie que l'exécution du programme sera bloquée jusqu'à ce que la vérification soit terminée. Pour éviter cela, il est possible d'utiliser la méthode asynchrone ```exists()```, qui utilise des callbacks pour traiter le résultat.

Il est également important de prendre en compte les différentes erreurs possibles lors de la vérification d'un répertoire, telles que l'absence de permissions pour y accéder, et de les gérer correctement dans le code.

## Voir aussi

Voici quelques liens utiles pour en savoir plus sur la vérification des répertoires en TypeScript:

- [Documentation officielle de la fonction existsSync()](https://nodejs.org/dist/latest-v12.x/docs/api/all.html#all_fs_existssync_path)

- [Tutorial sur la gestion des erreurs en TypeScript](https://www.digitalocean.com/community/tutorials/how-to-handle-errors-in-typescript)

- [Guide pour gérer les fichiers et les répertoires en Node.js](https://nodejs.org/api/fs.html)