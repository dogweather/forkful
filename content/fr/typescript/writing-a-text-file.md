---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire dans un fichier texte, c'est transcrire des données sous forme de texte. Les programmeurs le font pour enregistrer des configurations, des logs ou échanger des informations entre applications.

## How to:
TypeScript utilise Node.js pour écrire dans des fichiers. Installons `fs`, le module de système de fichiers.

```bash
npm install @types/node --save-dev
```

Créez une fonction `ecrireFichier` :

```typescript
import * as fs from 'fs';

function ecrireFichier(chemin: string, contenu: string): void {
  fs.writeFileSync(chemin, contenu);
}

// Utilisation
ecrireFichier('exemple.txt', 'Salut les devs TypeScript !');
```

Vérifiez `exemple.txt` :

```plaintext
Salut les devs TypeScript !
```

## Deep Dive
En TypeScript, l'écriture de fichiers repose surtout sur Node.js, introduit en 2009. D'autres bibliothèques, comme `fs-extra` ou des APIs browser-based telles que l'API `File`, offrent des fonctionnalités alternatifs. Pour garantir la compatibilité, utilisez des polyfills ou transpilez vers des standards supportés par les navigateurs ciblés.

## See Also
- Documentation Node.js fs: [nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- TypeScript Node Starter: [github.com/microsoft/TypeScript-Node-Starter](https://github.com/microsoft/TypeScript-Node-Starter)
- Mozilla Developer Network Web API interface | File: [developer.mozilla.org/en-US/docs/Web/API/File](https://developer.mozilla.org/en-US/docs/Web/API/File)
