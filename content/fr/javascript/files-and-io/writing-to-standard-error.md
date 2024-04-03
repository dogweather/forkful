---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:33.820463-07:00
description: "Comment faire : Dans Node.js, \xE9crire dans stderr peut \xEAtre accompli\
  \ en utilisant la m\xE9thode `console.error()` ou en \xE9crivant directement dans\u2026"
lastmod: '2024-03-13T22:44:58.294267-06:00'
model: gpt-4-0125-preview
summary: "Dans Node.js, \xE9crire dans stderr peut \xEAtre accompli en utilisant la\
  \ m\xE9thode `console.error()` ou en \xE9crivant directement dans `process.stderr`."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

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
