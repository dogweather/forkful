---
date: 2024-01-20 17:55:25.822613-07:00
description: 'How to: (Comment faire :) Lire un fichier texte en TypeScript est simple.
  Ici, on utilise `fs` de Node.js avec des promesses.'
lastmod: '2024-04-05T21:53:59.019123-06:00'
model: gpt-4-1106-preview
summary: (Comment faire :) Lire un fichier texte en TypeScript est simple.
title: Lecture d'un fichier texte
weight: 22
---

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
