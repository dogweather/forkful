---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:16.987396-07:00
description: "Comment faire : Dans un environnement Node.js, vous pouvez utiliser\
  \ le module int\xE9gr\xE9 `fs` (File System) pour \xE9crire des fichiers textes.\
  \ Cet exemple\u2026"
lastmod: '2024-03-13T22:44:58.296663-06:00'
model: gpt-4-0125-preview
summary: "Dans un environnement Node.js, vous pouvez utiliser le module int\xE9gr\xE9\
  \ `fs` (File System) pour \xE9crire des fichiers textes."
title: "R\xE9diger un fichier texte"
weight: 24
---

## Comment faire :
Dans un environnement Node.js, vous pouvez utiliser le module intégré `fs` (File System) pour écrire des fichiers textes. Cet exemple montre comment écrire du texte dans un fichier de manière asynchrone :

```javascript
const fs = require('fs');

const data = 'Bonjour le monde ! Ceci est du texte à écrire dans un fichier.';

fs.writeFile('exemple.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('Le fichier a été écrit.');
});
```

Sortie d’exemple :
```
Le fichier a été écrit.
```

Pour écrire un fichier de manière synchrone, utilisez `writeFileSync` :
```javascript
try {
  fs.writeFileSync('exemple.txt', data);
  console.log('Le fichier a été écrit.');
} catch (error) {
  console.error('Erreur lors de l’écriture du fichier :', error);
}
```

Dans les navigateurs web modernes, l'API d'accès au système de fichiers permet de lire et d'écrire des fichiers. Cependant, son utilisation est soumise aux permissions de l'utilisateur. Voici comment créer et écrire dans un fichier :

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Bonjour le monde ! Voici l’écriture de fichier dans le navigateur.');
  await writable.close();
}
```

Pour des scénarios plus complexes ou lors du travail avec de gros fichiers, vous pourriez opter pour des bibliothèques tierces comme `FileSaver.js` pour les navigateurs :

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Bonjour, Monde ! Ceci est du texte de FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "exemple.txt");
</script>
```

Rappelez-vous, l'écriture de fichiers côté client (dans les navigateurs) est restreinte en raison de préoccupations de sécurité, et toute opération nécessitant de sauvegarder sur le disque local de l'utilisateur requerra généralement leur permission explicite.
