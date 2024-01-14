---
title:                "TypeScript: Vérifier l'existence d'un répertoire"
simple_title:         "Vérifier l'existence d'un répertoire"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est important lors de la programmation de vérifier si un répertoire existe avant de l'utiliser. Cela peut éviter des erreurs ou des bogues dans votre code et améliorer l'efficacité de votre programme.

## Comment Faire

Pour vérifier si un répertoire existe en TypeScript, vous pouvez utiliser la méthode `existsSync` de la bibliothèque `fs`. Voici un exemple de code :

```TypeScript
import * as fs from 'fs';

if (fs.existsSync('/chemin/vers/le/répertoire')) {
  console.log('Le répertoire existe !');
} else {
  console.log('Le répertoire n\'existe pas !');
}
```

Si le répertoire existe, la console affichera "Le répertoire existe !". Sinon, elle affichera "Le répertoire n'existe pas !". Vous pouvez également utiliser cette méthode pour vérifier l'existence d'un fichier en remplaçant le chemin du répertoire par celui du fichier.

## Plongée en Profondeur

Si vous souhaitez en savoir plus sur la vérification de l'existence d'un répertoire, voici quelques points importants à noter :

- La méthode `existsSync` renvoie un booléen, ce qui signifie que vous pouvez l'utiliser dans une condition `if` comme dans l'exemple précédent.
- Cette méthode peut également être utilisée de manière asynchrone avec la méthode `exists` de la bibliothèque `fs` qui prend en paramètre un callback pour gérer le résultat de la vérification.
- Si vous souhaitez créer un nouveau répertoire, vous pouvez utiliser la méthode `mkdirSync` de la bibliothèque `fs` en vérifiant d'abord si le répertoire n'existe pas déjà.

## Voir Aussi

- [Documentation sur la méthode `existsSync` de la bibliothèque `fs`](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Documentation sur les méthodes `exists` et `mkdirSync` de la bibliothèque `fs`](https://nodejs.org/api/fs.html#fs_fs_exist_path_callback)
- [Article sur la gestion des erreurs en TypeScript](https://codeburst.io/error-handling-in-type-script-85a2b97a9ae2)