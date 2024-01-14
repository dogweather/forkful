---
title:    "TypeScript: Lecture d'un fichier texte"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire un fichier texte est une tâche courante en programmation et peut être utile pour de nombreuses raisons, notamment pour lire des configurations de logiciels ou pour analyser de grandes quantités de données stockées sous forme de texte.

## Comment faire

Pour lire un fichier texte en JavaScript, nous pouvons utiliser la méthode `readFile` du module `fs` intégré de Node.js. Cela nous permet de spécifier le chemin du fichier ainsi que l'encodage à utiliser. Par exemple :

```TypeScript
import { readFile } from 'fs';

readFile('monFichier.txt', 'utf-8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Dans cet exemple, nous utilisons `utf-8` comme encodage pour lire le fichier et afficher son contenu dans la console. Vous pouvez également utiliser d'autres options telles que `ascii` ou `base64`.

## Plonger plus profondément

Lorsque nous lisons un fichier texte en utilisant `readFile`, le contenu du fichier est retourné sous forme de chaîne de caractères. Cela peut être utile si vous souhaitez traiter le contenu du fichier en tant que texte brut. 

Cependant, si vous souhaitez effectuer des opérations plus sophistiquées sur le contenu du fichier, telles que le filtrage ou l'analyse, il peut être nécessaire d'utiliser des modules supplémentaires ou d'en savoir plus sur les formats de fichiers spécifiques.

## Voir aussi

- [Documentation sur `fs` in Node.js](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [Documentation sur la lecture de fichiers avec TypeScript](https://www.typescriptlang.org/docs/handbook/declaration-files/by-example.html#reading-file)
- [Tutoriel sur l'utilisation de `readFile` pour lire des fichiers texte en JavaScript](https://www.digitalocean.com/community/tutorials/reading-files-with-node-js)