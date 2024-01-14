---
title:    "TypeScript: Vérifier si un répertoire existe"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

La vérification de l'existence d'un répertoire est une étape importante dans le développement de tout projet en TypeScript. Cela permet de s'assurer que le répertoire nécessaire pour stocker des fichiers ou effectuer des opérations spécifiques est bien présent avant d'exécuter du code. Cela peut également aider à éviter des erreurs ou des bugs dans l'application finale. Dans cet article, nous allons expliquer comment vous pouvez facilement vérifier si un répertoire existe en utilisant TypeScript.

## Comment faire 

Pour vérifier si un répertoire existe en utilisant TypeScript, nous pouvons utiliser la méthode `existsSync` du module `fs`. Cette méthode prend un chemin de répertoire en paramètre et renvoie un booléen indiquant si le répertoire existe ou non. Regardons un exemple concret :

```
TypeScript
import * as fs from 'fs';

const directoryPath = 'chemin/vers/le/répertoire';

if (fs.existsSync(directoryPath)) {
    console.log('Le répertoire existe !');
} else {
    console.log('Le répertoire n\'existe pas.');
}
```

Dans cet exemple, nous importons le module `fs` pour pouvoir utiliser la méthode `existsSync`. Ensuite, nous déclarons le chemin du répertoire que nous voulons vérifier. En utilisant une simple condition, nous pouvons déterminer si le répertoire existe ou non.

Si le répertoire existe, nous affichons un message à l'utilisateur. Sinon, nous affichons un message d'erreur. Il est également possible d'utiliser une syntaxe `try-catch` pour gérer les potentielles erreurs lors de la vérification du répertoire.

## Deep Dive

Lors de l'utilisation de `existsSync` dans TypeScript, il est important de se rappeler que cette méthode ne fonctionne que pour les répertoires et non pour les fichiers. Cela signifie que si vous utilisez `existsSync` sur un fichier, cela renverra toujours `false`, même si le fichier existe réellement.

De plus, il est important de noter que la méthode `existsSync` est synchrone, ce qui signifie qu'elle bloquera l'exécution du code jusqu'à ce que la vérification soit terminée. Si vous préférez une approche asynchrone, vous pouvez utiliser la méthode `exists` du module `fs` en fournissant une fonction de rappel.

## Voir aussi

- [Documentation officielle de TypeScript](https://www.typescriptlang.org/docs/)
- [Documentation officielle de Node.js sur le module fs](https://nodejs.org/api/fs.html)
- [Article sur la gestion des fichiers et répertoires en TypeScript](https://medium.com/swlh/file-system-in-typescript-expressing-types-d60ee27bfd4d)