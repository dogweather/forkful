---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

En JavaScript, écrire dans l'erreur standard (`stderr`) permet de sortir des messages d'erreur sans interrompre le flux de données normales (`stdout`). Les programmeurs utilisent `stderr` pour diagnostiquer les problèmes sans mélanger les erreurs avec le contenu de sortie habituel.

## Comment faire :

```javascript
// Pour écrire dans stderr :
process.stderr.write("Un message d'erreur.\n");

// Example avec console.error :
console.error("Ceci est une erreur !");

// Lancer un process avec une sortie d'erreur personnalisee
const spawn = require('child_process').spawn;
const child = spawn('un_command_invalide');

child.stderr.on('data', (data) => {
  console.error(`Erreur depuis le processus enfant: ${data}`);
});
```

Sortie attendue dans la console:
```
Un message d'erreur.
Ceci est une erreur !
Erreur depuis le processus enfant: /bin/sh: un_command_invalide: command not found
```

## Exploration approfondie

Historiquement, diviser la sortie normale (`stdout`) et les erreurs (`stderr`) a facilité la redirection des sorties dans des fichiers ou d'autres dispositifs sans mélanger les contenus. On pourrait aussi utiliser `process.stdout` pour les données régulières, mais `stderr` a l'avantage d'être non bloquant et immédiat.

L'alternative à `stderr` est d'envoyer toutes les sorties via `stdout`, ce qui est moins pratique pour le débogage. L'implémentation de `stderr` dans Node.js utilise un flux `Writable` qui peut être redirigé, ce qui permet de capturer les erreurs pour les logs, sans interférer avec `stdout`.

## Voir aussi

- [La documentation Node.js sur les objets `console`](https://nodejs.org/api/console.html)
- [Stream API de Node.js](https://nodejs.org/api/stream.html)
- [Node.js process.stderr](https://nodejs.org/api/process.html#processstderr)