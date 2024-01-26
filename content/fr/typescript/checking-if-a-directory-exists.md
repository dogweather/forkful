---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:58:40.003744-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Vérifier l'existence d'un dossier, c'est chercher si un certain chemin sur le disque mène à un dossier réel. Les développeurs font ça pour éviter des erreurs en écriture/lecture ou pour créer des dossiers manquants.

## How to:

```TypeScript
import { promises as fs } from 'fs';

async function checkDirectoryExists(path: string): Promise<boolean> {
    try {
        await fs.access(path);
        return true;
    } catch {
        return false;
    }
}

// Utilisation:
const pathToCheck = '/chemin/vers/le/dossier';
checkDirectoryExists(pathToCheck)
    .then(exists => console.log(`Le dossier existe: ${exists}`))
    .catch(error => console.error('Erreur:', error));
```

Résultat possible:

```
Le dossier existe: true
```
ou en cas d'absence:

```
Le dossier existe: false
```

## Deep Dive

Historiquement, la vérification de l'existence d'un fichier ou d'un dossier se faisait via des modules comme `fs` de Node.js. Avant, on utilisait `fs.existsSync()`, mais cette méthode est bloquante et n'est pas recommandée dans un contexte asynchrone.

Alternatives: on pourrait utiliser `fs.stat()` ou `fs.readdir()` pour obtenir plus d'informations, mais ceci ajoute de la complexité.

Détails d'implémentation: `fs.access()` vérifie les permissions, ce qui suffit pour tester l'existence d'un dossier. Utiliser `promises as fs` permet d'avoir accès aux versions asynchrones et basées sur les promesses des méthodes de `fs`.

## See Also

- Documentation Node.js sur `fs` : [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- Article MDN sur les promesses : [https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Promise](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Guide sur async/await : [https://javascript.info/async-await](https://javascript.info/async-await)
