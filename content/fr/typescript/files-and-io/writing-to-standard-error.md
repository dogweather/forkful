---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:36.994591-07:00
description: "Comment faire : TypeScript, \xE9tant un sur-ensemble de JavaScript,\
  \ repose sur l'environnement d'ex\xE9cution JS sous-jacent (comme Node.js) pour\
  \ \xE9crire sur\u2026"
lastmod: '2024-03-13T22:44:57.454621-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, \xE9tant un sur-ensemble de JavaScript, repose sur l'environnement\
  \ d'ex\xE9cution JS sous-jacent (comme Node.js) pour \xE9crire sur stderr."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
TypeScript, étant un sur-ensemble de JavaScript, repose sur l'environnement d'exécution JS sous-jacent (comme Node.js) pour écrire sur stderr. Voici comment vous pouvez le faire directement :

```typescript
console.error("Ceci est un message d'erreur.");
```

Exemple de sortie sur stderr :
```
Ceci est un message d'erreur.
```

Dans un environnement Node.js, vous pouvez également utiliser la méthode `process.stderr.write()` pour une écriture de plus bas niveau :

```typescript
process.stderr.write("Message d'erreur de bas niveau.\n");
```

Exemple de sortie sur stderr :
```
Message d'erreur de bas niveau.
```

Pour une journalisation des erreurs plus structurée, vous pourriez utiliser des bibliothèques tierces populaires telles que `winston` ou `pino`. Voici comment journaliser des erreurs en utilisant `winston` :

D'abord, installez `winston` :

```bash
npm install winston
```

Ensuite, utilisez-le dans votre fichier TypeScript :

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Erreur journalisée en utilisant winston.');
```

Ceci écrira l'erreur à la fois sur la console et un fichier nommé `error.log`. Rappelez-vous, lors de l'écriture dans des fichiers, il est important de gérer les permissions des fichiers et le roulement pour prévenir les problèmes liés à l'utilisation de l'espace disque.
