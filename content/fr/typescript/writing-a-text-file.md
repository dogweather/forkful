---
title:    "TypeScript: Écrire un fichier texte"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte est une tâche courante dans le développement de logiciels. Que ce soit pour stocker des données, des configurations ou simplement pour enregistrer des informations importantes, l'écriture d'un fichier texte est essentielle pour tout programmeur.

## Comment faire

Pour écrire un fichier texte en TypeScript, nous pouvons utiliser la bibliothèque intégrée `fs` (file system). Tout d'abord, nous devons importer cette bibliothèque dans notre fichier TypeScript en utilisant la commande `import`:

```TypeScript
import * as fs from 'fs';
```

Ensuite, nous pouvons utiliser la fonction `writeFileSync()` pour écrire un fichier texte. Cette fonction prend deux paramètres : le nom du fichier et le contenu à écrire :

```TypeScript
fs.writeFileSync('monfichier.txt', 'Ceci est un fichier texte écrit en TypeScript.');
```

En exécutant ce code, nous créons un fichier texte appelé "monfichier.txt" avec le contenu spécifié. Si vous voulez ajouter du contenu à un fichier déjà existant, vous pouvez utiliser la fonction `appendFileSync()` :

```TypeScript
fs.appendFileSync('monfichier.txt', '\nVoici un autre texte ajouté à la fin.');
```

Il est important de noter que ces fonctions utilisent la synchronisation, ce qui signifie que le programme va attendre que l'opération d'écriture soit terminée avant de continuer. Cela peut être une bonne option pour des fichiers de petite taille, mais pour des fichiers plus grands, il est préférable d'utiliser des fonctions asynchrones pour éviter de bloquer le programme.

## Plongée en profondeur

Lorsque nous écrivons un fichier texte en TypeScript, nous pouvons également spécifier des options pour formater le contenu du fichier. Par exemple, nous pouvons définir le codage du fichier en utilisant la propriété `encoding` :

```TypeScript
fs.writeFileSync('monfichier.txt', 'Ceci est un texte avec des caractères spéciaux.', { encoding: 'utf8' });
```

De plus, la fonction `writeFileSync()` prend un troisième paramètre qui peut être utilisé pour spécifier différents modes d'écriture, comme `a+` pour ajouter du contenu sans écraser le fichier existant, ou `w+` pour écrire et lire.

En utilisant ces options, nous pouvons personnaliser notre écriture de fichier texte en fonction de nos besoins.

## Voir aussi

- [Documentation sur la bibliothèque `fs` en TypeScript](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)

- [Guide complet pour écrire et lire des fichiers en TypeScript](https://www.digitalocean.com/community/tutorials/js-reading-writing-files)

- [Utilisation de fonctions asynchrones avec `fs` en TypeScript](https://www.twilio.com/blog/asynchronous-file-i-o-in-typescript)