---
title:                "Vérifier si un répertoire existe"
html_title:           "TypeScript: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur, vous avez sûrement déjà rencontré le besoin de vérifier l'existence d'un dossier dans votre code. Cela peut être utile notamment dans le cas de la manipulation de fichiers ou de la gestion de chemin d'accès.

## Comment faire

Vous pouvez facilement vérifier si un dossier existe en utilisant la méthode `existsSync` de la classe `fs` dans TypeScript. Voici un exemple de code :

```TypeScript
import * as fs from 'fs';

// Vérifier l'existence du dossier "images"
if (fs.existsSync('images')) {
  console.log('Le dossier "images" existe.');
} else {
  console.log('Le dossier "images" n\'existe pas.');
}
```

Dans cet exemple, nous importons le module `fs` qui est fourni par Node.js pour gérer les fichiers et les dossiers. Ensuite, nous utilisons la méthode `existsSync` en lui passant en paramètre le nom du dossier que nous voulons vérifier. Si le dossier existe, la méthode renverra `true` sinon elle renverra `false`. Nous pouvons donc utiliser une condition pour afficher un message en fonction du résultat.

## Plongée en profondeur

La méthode `existsSync` utilise le système de fichiers du système d'exploitation pour vérifier l'existence du dossier. Elle renverra donc `true` si le dossier existe et si vous avez les permissions nécessaires pour y accéder. Cela peut varier selon le système d'exploitation sur lequel votre code est exécuté.

Il est également important de noter que la méthode `existsSync` peut être utilisée pour vérifier l'existence d'un fichier en passant en paramètre le chemin d'accès complet du fichier. Elle peut être utile pour vérifier si un fichier a été correctement créé ou supprimé dans votre code.

## Voir aussi

Pour en savoir plus sur la manipulation de fichiers en TypeScript, vous pouvez consulter ces ressources :

- [Documentation de la classe fs de Node.js](https://nodejs.org/api/fs.html)
- [Tutoriel sur la manipulation de fichiers en TypeScript](https://codeburst.io/learn-how-to-manage-files-with-typescript-in-nodejs-ed73c3b1a2c3)