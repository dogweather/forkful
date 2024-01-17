---
title:                "Lecture d'un fichier texte"
html_title:           "Javascript: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Lire un fichier texte en Javascript est un processus qui implique d'ouvrir et de parcourir un fichier afin d'en extraire des données. Cette tâche est souvent effectuée par les programmeurs pour traiter des données spécifiques, telles que des informations de configuration ou des données d'utilisateur.

## Comment faire:
Voici un exemple de code pour lire un fichier texte en utilisant le module intégré "fs" (système de fichiers) en Javascript.

```Javascript
const fs = require('fs'); // Importer le module

// Ouvrir le fichier en utilisant la fonction de lecture synchrone
// Ceci retourne un buffer (tampon) de données
const data = fs.readFileSync('nomDuFichier.txt');

// Convertir le buffer en chaîne de caractères en utilisant la méthode toString()
const text = data.toString();

// Afficher le contenu du fichier
console.log(text);
```

Le résultat affichera le contenu du fichier dans la console.

## Plongée en profondeur:
Lire des fichiers texte en Javascript n'est pas une tâche nouvelle. Avant les versions plus récentes de Javascript, cela nécessitait l'utilisation de modules externes tels que "fs-extra" ou "fsync". Cependant, depuis l'introduction de la norme ECMAScript 6 en 2015, la méthode `readFileSync()` est disponible en utilisant le module intégré "fs".

Il est également possible de lire des fichiers de manière asynchrone en utilisant la méthode `readFile()`, qui utilise des rappels (callbacks) pour traiter les données. De plus, il est recommandé d'utiliser des flux (streams) lors de la lecture de fichiers volumineux pour une meilleure performance.

## Veuillez noter:
Il est important de noter que l'accès et la lecture de fichiers sur un serveur distant (comme un navigateur web) peut être limité pour des raisons de sécurité. Ainsi, la lecture de fichiers en Javascript est principalement utilisée dans des environnements côté serveur tels que Node.js.

## Voir aussi:
Pour plus d'informations sur la lecture de fichiers en Javascript, vous pouvez consulter les ressources suivantes:
- Documentation sur le module "fs" de Node.js: https://nodejs.org/api/fs.html
- Tutoriel pour lire des fichiers en Javascript: https://www.w3schools.com/nodejs/nodejs_filesystem.asp