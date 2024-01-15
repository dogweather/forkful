---
title:                "La lecture d'un fichier texte"
html_title:           "TypeScript: La lecture d'un fichier texte"
simple_title:         "La lecture d'un fichier texte"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur TypeScript, vous avez probablement déjà entendu parler de la lecture de fichiers texte. Mais pourquoi devriez-vous vous intéresser à cela ? Eh bien, la lecture de fichiers texte peut être utile pour récupérer des données stockées dans un fichier, comme des paramètres de configuration ou des données de test. Cela peut également être utile pour traiter de grandes quantités de données dans un format facilement lisible.

## Comment procéder

Pour commencer, nous allons utiliser la fonction intégrée `fs` de Node.js pour lire un fichier texte. Tout d'abord, nous devons l'importer dans notre fichier TypeScript en utilisant `import fs from 'fs';`. Ensuite, nous pouvons utiliser la fonction `readFileSync()` pour lire le fichier et stocker son contenu dans une variable. Voici un exemple de code :

```TypeScript
import fs from 'fs';

let data = fs.readFileSync('monFichier.txt', 'utf-8');
```

Ensuite, nous pouvons utiliser la méthode `split()` pour séparer le contenu du fichier en un tableau, en utilisant un séparateur comme un saut de ligne ou une virgule. Par exemple :

```TypeScript
import fs from 'fs';

let data = fs.readFileSync('monFichier.txt', 'utf-8');
let tableau = data.split('\n');
```

Enfin, nous pouvons parcourir le tableau et traiter les données comme bon nous semble. Par exemple, si nos données étaient une liste de noms, nous pourrions les afficher dans la console en utilisant une boucle `for` :

```TypeScript
import fs from 'fs';

let data = fs.readFileSync('monFichier.txt', 'utf-8');
let tableau = data.split('\n');

for (let i = 0; i < tableau.length; i++) {
  console.log(tableau[i]);
}
```

Voici un exemple de sortie si notre fichier contient les noms "Alice", "Bob" et "Charlie" sur des lignes distinctes :

```
Alice
Bob
Charlie
```

## Plongée en profondeur

Maintenant que vous savez comment lire un fichier texte en TypeScript, nous allons plonger un peu plus profondément dans le processus. Lorsque nous utilisons la méthode `readFileSync()`, nous pouvons spécifier un paramètre facultatif pour définir l'encodage du fichier. Par défaut, il est défini sur `'utf-8'`, mais si votre fichier utilise un autre encodage tel que `'ascii'` ou `'latin1'`, vous devriez le spécifier pour éviter tout problème de lecture des caractères.

De plus, la méthode `readFileSync()` peut également prendre un deuxième paramètre pour spécifier des options telles que `encoding`, `flag` ou `signal`. Vous pouvez trouver plus d'informations sur ces options dans la documentation de Node.js.

## Voir aussi

- [Documentation officielle de Node.js sur la lecture de fichiers](https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options)
- [NodeJS Tuto - Lire et écrire dans un fichier avec Node.js](https://www.nodejs-tuto.com/08-nodejs-fs-readfilesync-writefilesync.html)