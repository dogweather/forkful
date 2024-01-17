---
title:                "Lecture d'un fichier texte"
html_title:           "TypeScript: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Lire un fichier texte en TypeScript consiste simplement à parcourir le contenu d'un fichier et à le stocker dans une variable. Les programmeurs utilisent cette méthode pour lire des fichiers de configuration, des données d'entrée ou des fichiers de log.

## Comment faire:

Lire un fichier texte en TypeScript est très simple grâce à la fonction `readFileSync` du module `fs`. Cette fonction prend en paramètre le chemin vers le fichier et renvoie son contenu sous forme de chaîne de caractères. Voici un exemple de code:

```TypeScript
import * as fs from 'fs';

const fileContent = fs.readFileSync('example.txt', 'utf8');
```

Ensuite, vous pouvez utiliser la variable `fileContent` pour manipuler le contenu du fichier comme bon vous semble. Par exemple, vous pouvez l'afficher sur la console avec `console.log(fileContent)` ou le traiter en tant que JSON avec `JSON.parse(fileContent)`.

## Plongée en profondeur:

Lire un fichier en TypeScript peut sembler banal, mais cela a été une fonctionnalité très demandée par la communauté jusqu'à la version 2.2 du langage. Avant cela, les programmeurs devaient utiliser du code supplémentaire ou des bibliothèques externes pour lire des fichiers.

Si vous avez besoin de lire un gros fichier, il serait plus efficace d'utiliser des bibliothèques dédiées comme `readline` ou `bufferedReader` plutôt que la méthode `readFileSync` qui utilise une ressource système importante.

## Voir aussi:

Pour en savoir plus sur la fonction `readFileSync` et le module `fs`, vous pouvez consulter la documentation officielle de TypeScript (https://www.typescriptlang.org/docs/) ou le module `fs` dans la documentation de Node.js (https://nodejs.org/api/fs.html). Vous pouvez également trouver de nombreux tutoriels sur Internet pour vous aider à lire un fichier en TypeScript selon vos besoins spécifiques.