---
title:                "Créer un fichier temporaire"
html_title:           "TypeScript: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Créer un fichier temporaire en programmation consiste simplement à créer un fichier qui ne durera que pour la durée d'une utilisation spécifique. Les programmeurs le font généralement pour stocker des données temporaires ou pour effectuer des opérations provisoires sans avoir à modifier des fichiers existants.

## Comment faire:

Voici un exemple de code TypeScript pour créer un fichier temporaire et y écrire des données:

```typescript
import { createWriteStream } from 'fs';

const tmpFile = createWriteStream('tempfile.txt');
tmpFile.write('Hello world!');
tmpFile.end();
```

La sortie sera un fichier texte nommé "tempfile.txt" contenant la phrase "Hello world!". Vous pouvez également spécifier un chemin différent en passant le chemin absolu ou relatif du fichier en tant que paramètre à la fonction `createWriteStream()`.

## Profondeur de plongée:

Les fichiers temporaires sont une pratique courante en programmation, utilisée depuis les premiers jours de l'informatique. Les alternatives comprennent l'utilisation de variables temporaires en mémoire, mais cela peut parfois entraîner une perte de données en cas d'erreur ou d'arrêt du programme. La création de fichiers temporaires offre une solution plus sécurisée et fiable.

En TypeScript, la fonction `createWriteStream()` appartient au module `fs` qui fait partie du noyau de Node.js. Cela signifie qu'il est disponible dès l'installation de Node.js, sans nécessiter d'autres modules ou packages.

## Voir aussi:

Pour en savoir plus sur la création de fichiers temporaires en TypeScript, consultez la documentation officielle de Node.js sur le module `fs` (https://nodejs.org/api/fs.html) ainsi que la documentation TypeScript sur l'utilisation des modules (https://www.typescriptlang.org/docs/handbook/modules.html).