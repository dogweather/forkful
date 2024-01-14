---
title:    "Javascript: Création d'un fichier temporaire"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi 

Lorsqu'on développe un programme en Javascript, il peut être utile de créer un fichier temporaire. Ce type de fichier a une durée de vie limitée et est généralement utilisé pour stocker des données de manière temporaire.

## Comment faire 

Il existe plusieurs façons de créer un fichier temporaire en Javascript. La première consiste à utiliser la méthode `fs.writeFileSync()`, qui permet à la fois de créer et d'écrire dans un fichier. Voici un exemple de code :

```Javascript
const fs = require('fs'); 

// création du fichier temporaire 
fs.writeFileSync('monfichier.temp', 'Contenu du fichier temporaire'); 

// lecture du fichier temporaire 
let data = fs.readFileSync('monfichier.temp', 'utf8'); 

// affichage du contenu 
console.log(data); 

/* output 
Contenu du fichier temporaire 
*/ 

// suppression du fichier 
fs.unlinkSync('monfichier.temp');
```

Il est également possible d'utiliser le module `tmp` pour créer un fichier temporaire. Voici un exemple de code :

```Javascript
const tmp = require('tmp'); 

// création du fichier temporaire 
let tempFile = tmp.fileSync(); 

// écriture dans le fichier temporaire 
fs.writeFileSync(tempFile.name, 'Contenu du fichier temporaire'); 

// lecture du fichier temporaire 
let data = fs.readFileSync(tempFile.name, 'utf8'); 

// affichage du contenu 
console.log(data); 

/* output 
Contenu du fichier temporaire 
*/ 

// suppression du fichier 
tempFile.removeCallback();
```

## Plongée en profondeur 

Créer un fichier temporaire peut être particulièrement utile lorsque l'on travaille avec des fichiers volumineux. En effet, cela permet de libérer de l'espace sur le disque dur en supprimant le fichier temporaire une fois qu'il n'est plus nécessaire.

Il est également important de prendre en compte la sécurité lors de la création de fichiers temporaires. Il est recommandé d'utiliser des noms de fichiers aléatoires et de s'assurer que le fichier temporaire est bien supprimé une fois qu'il n'est plus utilisé.

## Voir aussi

Pour en savoir plus sur la création de fichiers en Javascript, vous pouvez consulter les liens suivants :

- [Documentation Node.js sur la méthode `fs.writeFileSync()`](https://nodejs.org/dist/latest-v12.x/docs/api/fs.html#fs_fs_writefilesync_file_data_options)
- [Documentation du module `tmp`](https://www.npmjs.com/package/tmp)
- [Article sur la sécurité lors de la création de fichiers temporaires en Javascript](https://glebbahmutov.com/blog/secure-tmp-in-nodejs/)