---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi?

Créer un fichier temporaire, c'est créer un fichier qui n'est utilisé que brièvement pendant l'exécution d'un programme. Les programmeurs le font pour stocker temporairement des données sans encombrer la mémoire vive.

## Comment faire:

Voici comment créer un fichier temporaire en JavaScript avec le module `tmp-promise`. 

```Javascript
// Importe le module tmp-promise.
const tmp = require('tmp-promise');

// Crée un fichier temporaire.
tmp.file({ prefix: 'tmp-', postfix: '.txt' })
  .then(file => {
    console.log('Fichier temporaire créé:', file.path);
  })
  .catch(console.error);
```

Résultat attendu:

```Javascript
Fichier temporaire créé: /tmp/tmp-iu43f.txt
```

Pour écrire dans ce fichier:

```Javascript
// Importe les modules nécessaires.
const fs = require('fs').promises;
const tmp = require('tmp-promise');

tmp.file({ prefix: 'tmp-', postfix: '.txt' })
  .then(file =>
    fs.writeFile(file.fd, 'Salut monde!')
      .then(() => console.log('Données écrites dans le fichier:', file.path))
  )
  .catch(console.error);
```

Résultat attendu:

```Javascript
Données écrites dans le fichier: /tmp/tmp-iu43f.txt
```

## Approfondissement

Historiquement, les fichiers temporaires sont créés dans le répertoire `/tmp` sur les systèmes Unix, ce qui a été repris dans d'autres systèmes d'exploitation.

Une alternative pour stocker temporairement des données pourrait être d'utiliser la mémoire vive, mais cela peut être coûteux en termes d'espace et de performance. Dans certains cas, il serait plus efficace d'écrire les données dans un fichier temporaire et de le lire lorsque c'est nécessaire.

Quant à l'implémentation, le module `tmp-promise` que nous avons utilisé est basé sur le module` tmp` qui gère de manière transparente les fichiers temporaires et les répertoires dans node.js. 

## Voir aussi

- [Documentation de `tmp-promise`](https://www.npmjs.com/package/tmp-promise)
- [Fichiers temporaires sur Wikipedia](https://fr.wikipedia.org/wiki/Fichier_temporaire)
- [Module `fs` de Node.js pour travailler avec le système de fichiers](https://nodejs.org/api/fs.html).