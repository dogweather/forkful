---
title:    "Javascript: Vérification de l'existence d'un répertoire"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Pourquoi vérifier l'existence d'un répertoire en Javascript?

Avant de commencer à travailler sur un projet en Javascript, il est important de vérifier si un répertoire existe afin d'éviter des erreurs de lecture de fichiers ou de navigation dans un répertoire inexistant. Cela peut sembler trivial, mais cette étape est cruciale pour assurer le bon fonctionnement de votre code.

## Comment faire:

Pour vérifier l'existence d'un répertoire en Javascript, nous allons utiliser la méthode `existsSync` du module `fs` (file system). Cette méthode nous permet de vérifier si un fichier ou un répertoire existe en renvoyant une valeur booléenne `true` ou `false`. Voici un exemple de code:

```Javascript
const fs = require('fs');
const directory = 'images';

if (fs.existsSync(directory)) {
    console.log("Le répertoire existe!");
} else {
    console.log("Le répertoire n'existe pas!");
}
```

Dans cet exemple, nous importons le module `fs` et nous spécifions le nom du répertoire que nous souhaitons vérifier (`images`). Ensuite, grâce à la méthode `existsSync`, nous vérifions si ce répertoire existe. Si c'est le cas, nous affichons un message indiquant que le répertoire existe, sinon nous affichons un message indiquant le contraire.

## Approfondissement:

Maintenant que vous savez comment vérifier l'existence d'un répertoire en Javascript, il est important de comprendre que cette méthode renvoie également `true` si un fichier porte le même nom que le répertoire. Cela peut être utile si vous souhaitez vérifier si un fichier et un répertoire ont le même nom, mais si vous voulez spécifiquement vérifier l'existence d'un répertoire, il est important d'ajouter une condition supplémentaire pour vous assurer que le chemin spécifié est bien un répertoire. Voici un exemple de code:

```Javascript
const fs = require('fs');
const path = require('path');
const directory = 'images';

if (fs.existsSync(directory) && fs.lstatSync(directory).isDirectory()) {
    console.log("Le répertoire existe!");
} else {
    console.log("Le répertoire n'existe pas!");
}
```

Dans cet exemple, nous utilisons la méthode `lstatSync` du module `fs` pour obtenir des informations sur le chemin spécifié, puis nous utilisons la méthode `isDirectory` pour vérifier si c'est bien un répertoire.

## Voir aussi:

- Documentation sur la méthode `existsSync` : https://nodejs.org/api/fs.html#fs_fs_existssync_path
- Tutoriel sur la gestion des fichiers en Javascript : https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm
- Exemple de code pour créer un répertoire en Javascript : https://stackoverflow.com/questions/27351914/create-directory-if-not-exists-after-fs-writefile

Merci d'avoir lu cet article sur la vérification de l'existence d'un répertoire en Javascript. J'espère que cela vous sera utile dans vos projets futurs. N'hésitez pas à explorer davantage les méthodes du module `fs` pour mieux gérer les fichiers et répertoires dans vos applications. À bientôt!