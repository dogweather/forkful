---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:33.820463-07:00
description: "\xC9crire dans l'erreur standard (stderr) en JavaScript consiste \xE0\
  \ diriger les messages d'erreur ou toute information critique vers un flux sp\xE9\
  cifique et\u2026"
lastmod: '2024-03-13T22:44:58.294267-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire dans l'erreur standard (stderr) en JavaScript consiste \xE0 diriger\
  \ les messages d'erreur ou toute information critique vers un flux sp\xE9cifique\
  \ et\u2026"
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Quoi et pourquoi ?
Écrire dans l'erreur standard (stderr) en JavaScript consiste à diriger les messages d'erreur ou toute information critique vers un flux spécifique et séparé, ce qui est particulièrement utile dans les environnements de type Unix pour la journalisation et le débogage. Les programmeurs font cela pour différencier la sortie normale du programme des messages d'erreur, permettant une gestion plus claire de la sortie et une surveillance des erreurs plus facile.

## Comment faire :
Dans Node.js, écrire dans stderr peut être accompli en utilisant la méthode `console.error()` ou en écrivant directement dans `process.stderr`. Voici des exemples démontrant les deux approches :

```javascript
// Utilisation de console.error()
console.error('Ceci est un message d\'erreur.');

// Écriture directe dans process.stderr
process.stderr.write('Ceci est un autre message d\'erreur.\n');
```

La sortie d'échantillon pour les deux méthodes apparaîtrait dans le flux stderr, sans se mélanger avec stdout :
```
Ceci est un message d\'erreur.
Ceci est un autre message d\'erreur.
```

Pour une journalisation plus sophistiquée ou spécifique à l'application, de nombreux programmeurs JavaScript utilisent des bibliothèques tierces comme `winston` ou `bunyan`. Voici un rapide exemple utilisant `winston` :

D'abord, installez `winston` via npm :
```shell
npm install winston
```

Ensuite, configurez `winston` pour enregistrer les erreurs dans stderr :
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// Enregistrer un message d'erreur
logger.error('Erreur enregistrée par winston.');
```

Cette configuration garantit que lorsque vous enregistrez une erreur en utilisant `winston`, celle-ci est dirigée vers stderr, aidant à maintenir une séparation claire entre les sorties standard et d'erreur.
