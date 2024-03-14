---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:49.183859-07:00
description: "V\xE9rifier si un r\xE9pertoire existe en TypeScript est essentiel pour\
  \ les t\xE2ches de gestion de fichiers, telles que lire ou \xE9crire des donn\xE9\
  es dans des\u2026"
lastmod: '2024-03-13T22:44:57.452156-06:00'
model: gpt-4-0125-preview
summary: "V\xE9rifier si un r\xE9pertoire existe en TypeScript est essentiel pour\
  \ les t\xE2ches de gestion de fichiers, telles que lire ou \xE9crire des donn\xE9\
  es dans des\u2026"
title: "V\xE9rifier si un r\xE9pertoire existe"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Vérifier si un répertoire existe en TypeScript est essentiel pour les tâches de gestion de fichiers, telles que lire ou écrire des données dans des fichiers, en s'assurant que les opérations sont effectuées uniquement sur des répertoires valides. Cette opération est cruciale pour éviter les erreurs découlant de la tentative d'accès ou de manipulation de répertoires inexistants.

## Comment faire :

TypeScript, lorsqu'exécuté dans un environnement Node.js, vous permet de vérifier si un répertoire existe en utilisant le module `fs`, qui fournit la fonction `existsSync()` ou la fonction asynchrone `access()` combinée à `constants.F_OK`.

### Utiliser `fs.existsSync()` :

```typescript
import { existsSync } from 'fs';

const directoryPath = './chemin/vers/repertoire';

if (existsSync(directoryPath)) {
  console.log('Le répertoire existe.');
} else {
  console.log('Le répertoire n'existe pas.');
}
```

### Utiliser `fs.access()` avec `fs.constants.F_OK` :

```typescript
import { access, constants } from 'fs';

const directoryPath = './chemin/vers/repertoire';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Le répertoire n'existe pas.');
    return;
  }
  console.log('Le répertoire existe.');
});
```

**Exemple de sortie** pour les deux méthodes, en supposant que le répertoire existe :
```
Le répertoire existe.
```

Et s'il n'existe pas :
```
Le répertoire n'existe pas.
```

### Utiliser une librairie tierce - `fs-extra` :

`fs-extra` est une librairie tierce populaire qui améliore le module `fs` intégré et offre des fonctions plus pratiques.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './chemin/vers/repertoire';

pathExists(directoryPath).then(exists => {
  console.log(`Le répertoire existe : ${exists}`);
});
```

**Exemple de sortie** lorsque le répertoire existe :
```
Le répertoire existe : vrai
```

Et s'il n'existe pas :
```
Le répertoire existe : faux
```
