---
title:    "Javascript: Lecture d'un fichier texte"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi
La lecture de fichiers texte est une compétence fondamentale pour tout programmeur JavaScript. Cela vous permet de récupérer et de traiter des données externes pour les utiliser dans vos programmes. Que vous travailliez sur un projet personnel ou professionnel, savoir lire et manipuler correctement des fichiers texte est essentiel pour garantir le bon fonctionnement de votre code.

## Comment faire
Pour lire un fichier texte en JavaScript, vous devez utiliser des méthodes spécifiques de l'objet `fs` (système de fichiers). Tout d'abord, vous devez inclure le module natif `fs` dans votre code en utilisant `require()` :

```Javascript
const fs = require("fs");
```

Ensuite, vous pouvez utiliser la méthode `readFile()` pour lire un fichier. Elle prend deux arguments, le chemin du fichier et une fonction de rappel qui sera exécutée une fois que le fichier aura été lu. Voici un exemple de code :

```Javascript
fs.readFile("./example.txt", (err, data) => {
  if (err) throw err; // si une erreur se produit, elle sera affichée dans la console
  console.log(data); // affiche le contenu du fichier dans la console
});
```

La méthode `readFile()` renvoie un tableau de données brut, il est donc souvent nécessaire de le convertir en chaîne de caractères pour l'utiliser. Vous pouvez également spécifier un encodage dans le deuxième argument de la méthode `readFile()` pour éviter cette étape de conversion.

## Plongée en profondeur
La méthode `readFile()` est très utile pour lire de petits fichiers, mais il est important de prendre en compte les performances et la mémoire lorsque vous travaillez avec des fichiers de grande taille. Dans ces cas-là, vous pouvez utiliser la méthode `createReadStream()` pour lire des fichiers de manière plus efficace en utilisant les flux (stream). Elle prend également deux arguments, le chemin du fichier et un objet d'options, et renvoie un flux que vous pouvez ensuite manipuler pour lire et traiter les données du fichier.

## Voir aussi
- [Documentation officielle de la méthode `readFile()` (en anglais)](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [Documentation officielle de la méthode `createReadStream()` (en anglais)](https://nodejs.org/api/fs.html#fs_fs_createreadstream_path_options)
- [Guide pour lire et écrire des fichiers en JavaScript (en français)](https://www.pierre-giraud.com/javascript-apprendre-coder-cours/lire-ecrire-fichier/)