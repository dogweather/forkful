---
title:                "Lecture d'un fichier texte"
aliases:
- /fr/javascript/reading-a-text-file.md
date:                  2024-01-20T17:54:40.928876-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Lire un fichier texte en JavaScript, c'est récupérer le contenu d'un fichier pour le manipuler ou l'analyser. On le fait pour traiter des données, configurer une app, ou charger des scripts dynamiquement. 

## Comment faire :
Pour lire un fichier côté serveur (Node.js), utilisez le module `fs`:

```javascript
const fs = require('fs');

fs.readFile('exemple.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
});
```
Sortie attendue:
```
Contenu de votre fichier exemple.txt
```

Pour le navigateur, utilisez l'API `FileReader` :

```javascript
document.getElementById('input-file').addEventListener('change', function() {
  var reader = new FileReader();
  reader.onload = function() {
    console.log(reader.result);
  };
  reader.readAsText(this.files[0]);
});
```
HTML associé :
```html
<input type="file" id="input-file" />
```
Après avoir sélectionné un fichier, la console affichera son contenu.

## Exploration :
Historiquement, lire des fichiers avec JavaScript était limité au serveur (comme avec Node.js). Avec l'évolution des navigateurs et de l'HTML5, c'est désormais possible côté client avec l'API `FileReader`.

Il y a des alternatives, comme `fs.promises` pour une syntaxe async/await, ou `fetch` pour les fichiers statiques sur le serveur.

Côté implémentation, notez que `readFile` est asynchrone et peut bloquer avec de gros fichiers. Envisagez `readFileSync` pour les scripts simples ou `readFile` avec des streams pour de meilleurs performances.

## Voir aussi :
- Documentation Node.js 'fs': https://nodejs.org/api/fs.html
- MDN Web Docs 'FileReader': https://developer.mozilla.org/fr/docs/Web/API/FileReader
- Article sur les streams de Node.js: https://nodejs.org/api/stream.html
