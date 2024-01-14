---
title:                "TypeScript: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire des fichiers texte est une tâche courante pour un développeur TypeScript. Il est donc important de savoir comment le faire de manière efficace. Dans cet article, nous allons expliquer pourquoi il est utile de lire des fichiers texte et comment le faire en utilisant TypeScript.

## Comment faire

La première étape pour lire un fichier texte en TypeScript est d'ouvrir le fichier en utilisant la fonction `open()` de la classe `File`. La syntaxe est la suivante :

```
let file = new File("chemin/vers/fichier.txt", "r");
```

Ensuite, nous pouvons utiliser la méthode `readLine()` pour lire chaque ligne du fichier et stocker le résultat dans une variable. La syntaxe ressemble à ceci :

```
let line = file.readLine();
```

Enfin, pour afficher le contenu du fichier, nous pouvons utiliser la méthode `print()` de la console. Par exemple :

```
console.print(line);
```

## Profonde plongée

Lorsque nous utilisons la méthode `readLine()` pour lire un fichier, il est important de noter qu'elle ne lit qu'une seule ligne à la fois. Si nous voulons lire plusieurs lignes, nous devons créer une boucle pour appeler `readLine()` jusqu'à ce que la fin du fichier soit atteinte. Voici un exemple complet :

```
let file = new File("chemin/vers/fichier.txt", "r");

while(!file.EOF) {
  let line = file.readLine();
  console.print(line);
}

file.close();
```

## Voir aussi

- [Documentation TypeScript sur la classe File](https://www.typescriptlang.org/docs/handbook/utilities.html#class-file)
- [Guide pratique pour lire et écrire des fichiers en TypeScript](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-typescript) 
- [Exemple de code pour la lecture de fichiers en TypeScript](https://github.com/geraldoramosn/text-file-reader-typescript/blob/master/src/index.ts)