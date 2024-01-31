---
title:                "Lecture d'un fichier texte"
date:                  2024-01-20T17:55:25.822613-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Lire un fichier texte, c'est récupérer son contenu pour l'utiliser dans votre programme. Les programmeurs le font souvent pour charger des configurations, des données ou pour analyser du texte.

## How to: (Comment faire :)

Lire un fichier texte en TypeScript est simple. Ici, on utilise `fs` de Node.js avec des promesses.

```typescript
import { promises as fs } from 'fs';

async function readTextFile(filepath: string): Promise<string> {
  try {
    const data = await fs.readFile(filepath, 'utf8');
    return data;
  } catch (error) {
    throw new Error('Erreur lors de la lecture du fichier: ' + error.message);
  }
}

// Utilisation
const filepath = 'monFichier.txt';
readTextFile(filepath).then(console.log).catch(console.error);
```

La console affiche le contenu de `monFichier.txt` ou une erreur si la lecture échoue.

## Deep Dive (Plongée en profondeur)

Avant l’ère de Node.js et TypeScript, la lecture de fichiers était l’apanage de langages bas niveau comme C. Avec Node.js, cette opération devient native en JavaScript et, par extension, en TypeScript. 
Entre les méthodes synchrone, asynchrone avec callbacks et asynchrone avec promesses, la dernière est privilégiée pour son style plus lisible et moins pyramidale. C’est ce qu’on utilise dans l’exemple.
En alternatives, il y a des librairies comme `fs-extra` qui simplifient certaines opérations, ou le nouveau API `fs.promises` pour les promesses, plus moderne et propre.

Pour la lecture de gros fichiers, envisagez `fs.createReadStream`, qui lit par morceaux, empêchant ainsi de surcharger la mémoire.

## See Also (Voir aussi)

- Documentation Node.js sur `fs`: https://nodejs.org/api/fs.html
- Guide sur les stream Node.js: https://nodejs.org/api/stream.html
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
