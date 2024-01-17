---
title:                "Création d'un fichier temporaire"
html_title:           "Javascript: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Créer un fichier temporaire est une pratique courante dans la programmation, qui consiste à créer un fichier qui est destiné à être utilisé et supprimé immédiatement après son utilisation. Les programmeurs le font souvent pour stocker des données temporaires ou pour exécuter des actions spécifiques sans altérer les fichiers existants.

## Comment faire:

Voici un exemple de code en Javascript montrant comment créer un fichier temporaire:

```Javascript
const fs = require('fs');
const path = require('path');

// Génère un nom de fichier temporaire unique
const tempFileName = `tempFile_${Math.floor(Math.random() * 10000)}`;

// Crée un fichier temporaire vide
fs.writeFile(path.join(__dirname, tempFileName), '', (error) => {
  if (error) throw error;
  console.log(`Fichier temporaire ${tempFileName} créé avec succès!`)
});
```

Lorsque vous exécutez ce code, un nouveau fichier temporaire vide sera créé et vous pourrez y ajouter vos données ou y exécuter des actions. Une fois le fichier utilisé, il sera automatiquement supprimé grâce à la méthode `fs.writeFile()` en spécifiant un nom de fichier temporaire unique.

## Plongée en profondeur:

Les fichiers temporaires sont utilisés depuis longtemps en programmation pour différentes raisons, telles que la gestion de fichiers temporaires lors de l'installation de logiciels ou pour stocker des données intermédiaires lors de la compilation de code. Les alternatives à la création de fichiers temporaires incluent l'utilisation de variables en mémoire ou de bases de données temporaires.

L'implémentation exacte de la création de fichiers temporaires peut varier selon le langage de programmation utilisé. En Javascript, la méthode `fs.writeFile()` est largement utilisée pour créer des fichiers temporaires, mais il existe également des bibliothèques tierces telles que `tmp` qui facilitent la création de ces fichiers.

## Voir aussi:

- Documentation officielle de `fs.writeFile()` en Node.js: https://nodejs.org/api/fs.html#fs_fs_writefile_path_data_options_callback
- Bibliothèque tierce `tmp` pour gérer les fichiers temporaires en Javascript: https://www.npmjs.com/package/tmp
- Un tutoriel sur la création de fichiers temporaires en Javascript: https://alligator.io/nodejs/temporary-files/