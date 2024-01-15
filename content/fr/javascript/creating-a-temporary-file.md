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

## Pourquoi

Créer un fichier temporaire peut être utile dans plusieurs cas, tels que le traitement de données sensibles, la gestion de fichiers temporaires pour des opérations en cours, ou la mise en cache de données pour améliorer les performances. En utilisant du code JavaScript, vous pouvez facilement créer et manipuler des fichiers temporaires pour répondre à vos besoins spécifiques.

## Comment faire

Pour créer un fichier temporaire en JavaScript, il faut d'abord utiliser la méthode `createTempFile()` de l'objet `FileSystem` ou `fs` (pour les versions plus récentes de Node.js). Cette méthode prend en paramètre le chemin vers le répertoire dans lequel vous souhaitez créer le fichier temporaire, ainsi qu'un callback optionnel pour manipuler le fichier une fois créé.

Voici un exemple qui crée un fichier temporaire dans le répertoire "temp" et y écrit du contenu :

```javascript
var fs = require('fs');
fs.createTempFile('temp', function(err, tempFile) {
  if (err) throw err;
  // Écriture du contenu dans le fichier
  fs.writeFile(tempFile, 'Contenu temporaire', function(err) {
    if (err) throw err;
    console.log('Fichier temporaire créé et contenu ajouté.');
  });
});
```

Vous pouvez également spécifier une extension de fichier optionnelle pour le fichier temporaire en utilisant l'option `suffix` dans la méthode `createTempFile()`. Par exemple :

```javascript
var fs = require('fs');
fs.createTempFile('temp', { suffix: '.txt' }, function(err, tempFile) {
  if (err) throw err;
  // Écriture du contenu dans le fichier
  fs.writeFile(tempFile, 'Contenu temporaire', function(err) {
    if (err) throw err;
    console.log('Fichier temporaire créé et contenu ajouté.');
  });
});
```

## Plongée en profondeur

Il est important de noter que les fichiers temporaires créés avec la méthode `createTempFile()` seront automatiquement supprimés lorsque le processus se termine ou si le fichier est fermé avec la méthode `close()`. De plus, en utilisant les méthodes de l'objet `fs`, vous pouvez facilement modifier, déplacer ou supprimer un fichier temporaire selon vos besoins.

Il est également intéressant de mentionner que la création de fichiers temporaires peut être un moyen efficace de limiter l'utilisation d'espace disque dans le cas où votre application nécessite de stocker des données temporaires pendant une courte période de temps.

## Voir aussi

- [Documentation sur la méthode `createTempFile()`](https://nodejs.org/dist/latest-v16.x/docs/api/fs.html#fs_fs_createtempfile_prefix_options_callback)
- [Tutoriel sur la manipulation de fichiers en JavaScript](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Exemples d'utilisation de fichiers temporaires en Node.js](https://www.codegrepper.com/code-examples/javascript/nodejs+create+temp+file)