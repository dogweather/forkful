---
title:                "Vérifier si un répertoire existe"
html_title:           "Javascript: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

Vous vous demandez peut-être pourquoi quelqu'un voudrait vérifier l'existence d'un répertoire en utilisant du Javascript. Il y a plusieurs raisons à cela, mais l'une des principales est de s'assurer que votre code fonctionne correctement en évitant les erreurs lorsque vous essayez d'accéder à un répertoire qui n'existe pas.

# Comment faire

```Javascript
if (fs.existsSync(pathToDirectory)) {
  console.log("Le répertoire existe!");
} else {
  console.log("Le répertoire n'existe pas!");
}
```

Dans cet exemple, nous utilisons la méthode `existsSync()` de l'objet `fs` pour vérifier si un répertoire existe en fournissant le chemin d'accès au répertoire en tant que paramètre. Si le répertoire existe, la première instruction `console.log()` sera exécutée, sinon la deuxième instruction sera exécutée. Vous pouvez également utiliser cette méthode avec une déclaration `if/else` pour effectuer différentes actions en fonction de l'existence ou non du répertoire.

# Approfondissement

La méthode `existsSync()` appartient à l'objet `fs` (filesystem) qui est un module intégré de Node.js. Elle permet de vérifier si un chemin d'accès existe avant d'y accéder, ce qui peut être utile dans de nombreux cas, comme la lecture ou l'écriture de fichiers dans un répertoire spécifique.

Il est important de noter que cette méthode ne vérifie que l'existence d'un chemin d'accès et non pas spécifiquement d'un répertoire. Si le chemin d'accès fourni correspond à un fichier et non pas à un répertoire, la méthode renverra également `true`. Si vous souhaitez vérifier l'existence d'un répertoire spécifique, vous devez d'abord vous assurer que le chemin d'accès fourni correspond bien à un répertoire en utilisant la méthode `fs.lstatSync()`. 

# Voir aussi
- [Documentation officielle de Node.js sur l'objet fs](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [Article sur la méthode existsSync() de l'objet fs](https://www.geeksforgeeks.org/node-js-fs-existssync-method/)
- [Exemple de projet utilisant la méthode existsSync() pour vérifier l'existence d'un répertoire](https://github.com/TheBokiya/javascript-check-directory-exists)