---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:46.602487-07:00
description: "Comment : En Node.js, puisque JavaScript n'a pas directement acc\xE8\
  s au syst\xE8me de fichiers, le module `fs` est g\xE9n\xE9ralement utilis\xE9 pour\
  \ de telles\u2026"
lastmod: '2024-03-13T22:44:58.291108-06:00'
model: gpt-4-0125-preview
summary: "En Node.js, puisque JavaScript n'a pas directement acc\xE8s au syst\xE8\
  me de fichiers, le module `fs` est g\xE9n\xE9ralement utilis\xE9 pour de telles\
  \ op\xE9rations."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

## Comment :
En Node.js, puisque JavaScript n'a pas directement accès au système de fichiers, le module `fs` est généralement utilisé pour de telles opérations. Voici une manière simple de vérifier si un répertoire existe en utilisant `fs.existsSync()` :

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Vérifier si le répertoire existe
if (fs.existsSync(directoryPath)) {
  console.log('Le répertoire existe.');
} else {
  console.log('Le répertoire n'existe pas.');
}
```
**Exemple de Sortie :**
```
Le répertoire existe.
```
Ou, pour une approche asynchrone non bloquante, utilisez `fs.promises` avec `async/await` :

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('Le répertoire existe.');
  } catch (error) {
    console.log('Le répertoire n'existe pas.');
  }
}

checkDirectory('./sample-directory');
```
**Exemple de Sortie :**
```
Le répertoire existe.
```

Pour les projets qui utilisent intensivement les opérations sur les fichiers et les répertoires, le package `fs-extra`, une extension du module natif `fs`, offre des méthodes supplémentaires pratiques. Voici comment vous pouvez réaliser la même chose avec `fs-extra` :

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Vérifier si le répertoire existe
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'Le répertoire existe.' : 'Le répertoire n'existe pas.'))
  .catch(err => console.error(err));
```
**Exemple de Sortie :**
```
Le répertoire existe.
```

Cette approche permet un code propre et lisible qui s'intègre parfaitement aux pratiques modernes de JavaScript.
