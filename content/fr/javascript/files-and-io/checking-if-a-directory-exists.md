---
title:                "Vérifier si un répertoire existe"
aliases:
- /fr/javascript/checking-if-a-directory-exists/
date:                  2024-02-03T19:07:46.602487-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Vérifier si un répertoire existe en JavaScript est essentiel pour les tâches de manipulation de fichiers, permettant aux scripts de vérifier la présence du répertoire avant de le lire ou d'y écrire. Cette opération prévient les erreurs et assure une exécution plus fluide du programme, en particulier dans les applications qui gèrent dynamiquement les fichiers ou les répertoires en fonction des entrées des utilisateurs ou des sources de données externes.

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
