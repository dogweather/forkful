---
title:                "Javascript: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous vous demandez peut-être pourquoi vous devriez vous intéresser à vérifier l'existence d'un répertoire en Javascript. Eh bien, cela peut être utile si vous devez accéder à des fichiers ou des dossiers spécifiques dans votre code. Cela peut vous éviter des erreurs et vous permettre de manipuler facilement vos fichiers.

## Comment faire
Pour vérifier si un répertoire existe en Javascript, vous pouvez utiliser la méthode ```fs.existsSync()```. Prenons un exemple concret :

```Javascript
const fs = require('fs'); // import du module fs

// vérifie si le répertoire "images" existe
if (fs.existsSync('./images')) {
  console.log('Le répertoire images existe !');
} else {
  console.log('Le répertoire images n\'existe pas.');
}

// affichage du contenu du répertoire "images"
console.log(fs.readdirSync('./images'));
```

Output :

```
Le répertoire images existe !
[ 'image1.jpg', 'image2.png', 'image3.gif' ]
```

Vous pouvez également utiliser la méthode ```fs.statSync()``` pour vérifier si un chemin spécifique est un répertoire ou non. Voici un exemple :

```Javascript
const fs = require('fs'); // import du module fs

// vérifie si "images/image2.png" est un répertoire
const stats = fs.statSync('./images/image2.png');
if (stats.isDirectory()) {
  console.log('"images/image2.png" est un répertoire.');
} else {
  console.log('"images/image2.png" n\'est pas un répertoire.');
}
```

Output :

```
"images/image2.png" n'est pas un répertoire.
```

## Deep Dive
Maintenant que vous savez comment vérifier si un répertoire existe en Javascript, vous pouvez également utiliser la méthode ```fs.accessSync()``` pour vérifier la permission d'accès à un répertoire. Cette méthode accepte deux arguments : le chemin du répertoire et une constante qui spécifie le type d'accès (lecture, écriture ou exécution). Voici un exemple :

```Javascript
const fs = require('fs'); // import du module fs

// vérifie si nous avons la permission de lecture sur "documents"
fs.accessSync('./documents', fs.constants.R_OK, (err) => {
  if (err) {
    console.log('Vous n\'avez pas la permission sur "documents".');
  } else {
    console.log('Vous avez la permission sur "documents".');
  }
});
```

Output :

```
Vous avez la permission sur "documents".
```

## Voir aussi
- Documentation officielle de Node.js sur la méthode ```fs.existsSync()``` : https://nodejs.org/api/fs.html#fs_fs_existssync_path
- Tutoriel sur l'utilisation des méthodes ```fs.existsSync()``` et ```fs.statSync()``` : https://www.geeksforgeeks.org/node-js-fs-stats-method/
- Tutoriel sur la méthode ```fs.accessSync()``` : https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm