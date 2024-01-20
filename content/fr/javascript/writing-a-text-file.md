---
title:                "Écriture d'un fichier texte"
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire un fichier texte en JavaScript, c'est sauvegarder des données en format lisible par l'homme. C'est utile pour créer des logs, exporter des données, et pour configurer des apps avec des fichiers comme `.env`.

## Comment faire :

```javascript
const fs = require('fs');

// Écrire du texte dans un nouveau fichier 
fs.writeFile('message.txt', 'Bonjour le monde!', (err) => {
  if (err) throw err;
  console.log('Le fichier a été sauvegardé!');
});

// Ajouter du texte à un fichier existant
fs.appendFile('message.txt', '\nAu revoir!', (err) => {
  if (err) throw err;
  console.log('Le texte a été ajouté!');
});
```
Sortie :
```
Le fichier a été sauvegardé!
Le texte a été ajouté!
```

## Plongée Profonde

Historiquement, l'interaction avec le système de fichiers était effectuée côté serveur. Avec Node.js, on manipule ces fichiers avec le module `fs`. Il y a des alternatives comme les file systems promisified (`fs.promises`) pour écrire avec des Promises/async-await. Concernant les navigateurs, l'API File et FileReader interagissent avec les fichiers locaux mais avec des restrictions de sécurité.

## Voir Également

- Documentation Node.js sur `fs`: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- MDN Web Docs sur l'API File: [https://developer.mozilla.org/en-US/docs/Web/API/File](https://developer.mozilla.org/en-US/docs/Web/API/File)
- MDN Web Docs sur FileReader: [https://developer.mozilla.org/en-US/docs/Web/API/FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)