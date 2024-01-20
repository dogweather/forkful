---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:57:06.766831-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Vérifier l'existence d'un dossier, c'est juste contrôler si un chemin donné pointe sur un truc réel sur votre machine. On fait ça pour éviter des erreurs quand on tente de lire ou d’écrire dedans.

## How to:
En JavaScript (avec Node.js), on utilise les modules `fs` et `fs/promises` pour toucher au système de fichiers. Voilà comment on vérifie si un dossier existe :

```javascript
const fs = require('fs');

// Version synchrone
const dossierExiste = fs.existsSync('/chemin/du/dossier'); // retourne true ou false
console.log('Le dossier existe:', dossierExiste);

// Version asynchrone (avec Promises)
const fsPromises = require('fs').promises;
const cheminDossier = '/chemin/du/dossier';

fsPromises.access(cheminDossier, fs.constants.F_OK)
  .then(() => console.log('Le dossier existe.'))
  .catch(() => console.log('Le dossier n’existe pas.'));
```

## Deep Dive
Historiquement, on se servait de `fs.exists`, mais Node.js l’a déprécié car c’était une source de problèmes, surtout avec son style non standard de callback. On préfère maintenant `fs.existsSync` ou `fs.access` avec `fs.constants.F_OK`, qui vérifie les droits d'accès au dossier.

Alternatives? Y'en a pas vraiment des masses. Ou tu fais avec le système `fs` de Node, ou tu passes par un outil externe avec `child_process`. Mais franchement, `fs` suffit dans la plupart des cas.

Concernant les détails d'implémentation, `fs.existsSync` bloque le fil d’exécution jusqu'à ce qu'il ait fini. Bon à savoir si tu codes quelque chose très sensible au temps. Pour les applications sensibles à la performance, la version asynchrone avec `fsPromises.access` est préférable.

## See Also
- Node.js `fs` docs: https://nodejs.org/api/fs.html
- À propos de `fsPromises.access`: https://nodejs.org/api/fs.html#fspromisesaccesspath-mode
- Une discussion sur la dépréciation de `fs.exists`: https://github.com/nodejs/node/issues/103
- Pour exécuter des commandes shell, `child_process`: https://nodejs.org/api/child_process.html