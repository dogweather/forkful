---
title:    "TypeScript: Ecrire un fichier texte"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture d'un fichier texte est une compétence essentielle pour tout programmeur TypeScript. Cela permet de stocker et de manipuler les données de manière plus efficace et de les rendre lisibles pour les autres utilisateurs. Il est également utile pour créer des fichiers de configuration ou de documentation pour un projet.

## Comment faire

Pour écrire un fichier texte en TypeScript, il y a quelques étapes simples à suivre :

1. Importez le module `fs` en utilisant la commande `import * as fs from 'fs';`
2. Définissez le contenu du fichier à l'aide d'une chaîne de caractères.
3. Utilisez la méthode `writeFileSync` du module `fs` pour écrire le contenu dans un fichier.
4. Spécifiez le chemin et le nom du fichier dans les paramètres de la méthode `writeFileSync`.

Voici un exemple de code pour écrire un fichier texte appelé "nouveauFichier.txt" avec le contenu "Bonjour le monde !" dans le répertoire actuel :

```TypeScript
import * as fs from 'fs';

const nouveauFichier = 'Bonjour le monde !';
fs.writeFileSync('./nouveauFichier.txt', nouveauFichier);
```

Une fois exécuté, ce code créera un fichier texte nommé "nouveauFichier.txt" dans le répertoire actuel avec le contenu "Bonjour le monde !".

## Deep Dive

Les fichiers texte peuvent contenir bien plus que du simple texte. En utilisant différentes méthodes fournies par le module `fs`, il est possible de manipuler les fichiers texte pour effectuer différentes tâches telles que la lecture, la suppression ou la mise à jour de leur contenu.

Parmi ces méthodes, on peut citer `readFileSync` pour lire le contenu d'un fichier, `unlinkSync` pour supprimer un fichier, ou encore `appendFileSync` pour ajouter du contenu à un fichier déjà existant.

## Voir aussi

- [Documentation officielle de TypeScript sur le module fs](https://www.typescriptlang.org/docs/handbook/fs.html)
- [Tutoriel sur l'écriture de fichiers en TypeScript](https://stackabuse.com/writing-files-using-node-js/)
- [Exemples de manipulation de fichiers avec TypeScript](https://github.com/Bogdan-Lyashenko/Under-the-hood-NodeJS/blob/master/Under-the-hood-NodeJS.pdf) (en anglais)