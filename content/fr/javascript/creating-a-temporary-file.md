---
title:                "Javascript: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer des fichiers temporaires?

Créer des fichiers temporaires est une pratique courante en programmation, car cela offre de nombreux avantages. Tout d'abord, cela permet de stocker des données temporaires qui seront utilisées uniquement pendant l'exécution du programme. De plus, cela permet de libérer de l'espace mémoire en supprimant ces fichiers une fois qu'ils ne sont plus nécessaires.

## Comment créer des fichiers temporaires en Javascript

Il existe plusieurs façons de créer des fichiers temporaires en Javascript, en voici quelques exemples :

```Javascript
//Méthode 1 : Utilisation de la méthode fs.writeFileSync() de Node.js
const fs = require('fs');
fs.writeFileSync('fichiertemporaire.txt', 'Contenu du fichier temporaire');

//Méthode 2 : Utilisation de la bibliothèque tempy
const tempy = require('tempy');
const chemin = tempy.file({extension: 'txt'});
fs.writeFileSync(chemin, 'Contenu du fichier temporaire');

//Méthode 3 : Utilisation de la bibliothèque os.tmpdir()
const os = require('os');
const chemin = os.tmpdir() + '/fichiertemporaire.txt';
fs.writeFileSync(chemin, 'Contenu du fichier temporaire');
```

### Résultat de l'exécution de ces exemples

Dans nos exemples, nous avons créé un fichier temporaire nommé "fichiertemporaire.txt" avec du contenu à l'intérieur. Le fichier temporaire sera automatiquement supprimé à la fin de l'exécution du programme.

## Plongée en profondeur

Pour ceux qui souhaitent comprendre davantage le processus de création de fichiers temporaires en Javascript, voici quelques points à prendre en compte :

- La bibliothèque tempy utilise la méthode fs.mkdtempSync() pour créer un dossier temporaire, puis y écrit un fichier à l'intérieur. Cela permet de garantir que le fichier sera bien supprimé, même si une erreur se produit entre la création du dossier et l'écriture du fichier à l'intérieur.
- La méthode os.tmpdir() de Node.js renvoie le chemin du dossier temporaire par défaut de l'utilisateur. Cela peut varier selon les systèmes d'exploitation et les préférences de l'utilisateur.
- Il est important de bien gérer la suppression des fichiers temporaires pour éviter une surcharge de l'espace de stockage. La méthode fs.unlinkSync() peut être utilisée pour supprimer un fichier en utilisant son chemin.

## Voir aussi

- [Documentation de la méthode fs.writeFileSync()](https://nodejs.org/api/fs.html#fs_fs_writefilesync_file_data_options)
- [Documentation de la bibliothèque tempy](https://www.npmjs.com/package/tempy)
- [Documentation de la méthode os.tmpdir()](https://nodejs.org/api/os.html#os_os_tmpdir)