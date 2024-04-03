---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:32.618469-07:00
description: "Comment faire : TypeScript en lui-m\xEAme ne g\xE8re pas directement\
  \ les op\xE9rations sur les fichiers car il se compile en JavaScript, qui traditionnellement\u2026"
lastmod: '2024-03-13T22:44:57.456676-06:00'
model: gpt-4-0125-preview
summary: "TypeScript en lui-m\xEAme ne g\xE8re pas directement les op\xE9rations sur\
  \ les fichiers car il se compile en JavaScript, qui traditionnellement s'ex\xE9\
  cute dans le navigateur avec un acc\xE8s limit\xE9 au syst\xE8me de fichiers."
title: "R\xE9diger un fichier texte"
weight: 24
---

## Comment faire :
TypeScript en lui-même ne gère pas directement les opérations sur les fichiers car il se compile en JavaScript, qui traditionnellement s'exécute dans le navigateur avec un accès limité au système de fichiers. Cependant, lorsqu'il est utilisé dans un environnement Node.js, le module `fs` (Système de Fichiers) fournit la fonctionnalité d'écriture de fichiers.

### Utilisation du module fs de Node.js
Tout d'abord, assurez-vous de travailler dans un environnement Node.js. Ensuite, utilisez le module `fs` pour écrire des fichiers texte. Voici un exemple basique :

```typescript
import * as fs from 'fs';

const data = 'Bonjour, monde !';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('Le fichier a été enregistré !');
});
```

Cela écrira de manière asynchrone "Bonjour, monde !" dans `message.txt`. Si le fichier n'existe pas, Node.js le crée ; s'il existe déjà, Node.js l'écrase.

Pour une écriture de fichier synchrone, utilisez `writeFileSync` :

```typescript
import * as fs from 'fs';

const data = 'Bonjour encore, monde !';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('Le fichier a été enregistré !');
} catch (err) {
    console.error(err);
}
```

### Utilisation de bibliothèques tierces populaires
Bien que le module `fs` natif soit puissant, certains développeurs préfèrent utiliser des bibliothèques tierces pour plus de commodité et de fonctionnalités. `fs-extra` est un choix populaire qui étend `fs` et rend les opérations sur les fichiers plus simples.

Tout d'abord, vous devez installer `fs-extra` :

```
npm install fs-extra
```

Ensuite, vous pouvez l'utiliser dans votre fichier TypeScript pour écrire du contenu texte :

```typescript
import * as fs from 'fs-extra';

const data = 'Ceci est fs-extra !';
const filePath = './extraMessage.txt';

// Utilisation de async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('Le fichier a été enregistré avec fs-extra !');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

Ce fragment de code fait la même chose que les exemples `fs` précédents mais utilise la bibliothèque `fs-extra`, offrant une syntaxe plus propre pour la gestion des promesses.
