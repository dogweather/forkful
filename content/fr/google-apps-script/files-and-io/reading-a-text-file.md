---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:08.475940-07:00
description: "Lire un fichier texte avec Google Apps Script (GAS) implique d'acc\xE9\
  der et d'extraire des donn\xE9es textuelles \xE0 partir de fichiers stock\xE9s dans\
  \ Google Drive\u2026"
lastmod: '2024-03-13T22:44:57.207206-06:00'
model: gpt-4-0125-preview
summary: "Lire un fichier texte avec Google Apps Script (GAS) implique d'acc\xE9der\
  \ et d'extraire des donn\xE9es textuelles \xE0 partir de fichiers stock\xE9s dans\
  \ Google Drive ou d'autres stockages bas\xE9s sur le cloud accessibles."
title: Lecture d'un fichier texte
weight: 22
---

## Comment faire :
Pour commencer à lire un fichier texte avec Google Apps Script, vous devez généralement utiliser l'API Google Drive. Voici un exemple simple montrant comment lire un fichier depuis Google Drive :

```javascript
function readFileContents(fileId) {
  // Obtient le fichier Google Drive par son ID
  var file = DriveApp.getFileById(fileId);
  
  // Obtient les données blob sous forme de texte
  var text = file.getBlob().getDataAsString();
  
  // Enregistre le contenu dans le journal de Google Apps Script
  Logger.log(text);
  return text;
}
```

*Exemple de sortie dans le journal :*

```
Bonjour, monde ! Ceci est un fichier texte de test.
```

Dans cet exemple, `fileId` est l'identifiant unique du fichier que vous souhaitez lire. Le service `DriveApp` récupère le fichier, et `getDataAsString()` lit son contenu sous forme de chaîne de caractères. Vous pouvez ensuite manipuler ou utiliser ce texte comme nécessaire.

## Plongée profonde
Historiquement, lire des fichiers texte dans des applications web, comme celles construites avec Google Apps Script, présentait des défis en raison des restrictions de sécurité des navigateurs et de la nature asynchrone de JavaScript. Google Apps Script simplifie cela avec ses services abstraits tels que `DriveApp`, fournissant une API de haut niveau pour interagir avec les fichiers Google Drive.

Cependant, une considération importante est la performance et les limites de temps d'exécution imposées par Google Apps Script, surtout lors de la lecture de gros fichiers ou de l'exécution d'opérations complexes avec les données. Dans certains cas, il pourrait être plus efficace d'utiliser directement les services Google Cloud depuis un backend plus puissant ou de prétraiter les fichiers en morceaux plus gérables.

Pour le traitement complexe des fichiers ou lorsque la performance en temps réel est critique, des alternatives telles que Google Cloud Functions, qui supporte Node.js, Python et Go, pourraient offrir plus de flexibilité et de ressources informatiques. Néanmoins, pour des tâches simples au sein de l'écosystème Google, surtout là où la simplicité et la facilité d'intégration avec les produits Google sont primordiales, Google Apps Script fournit une approche remarquablement conviviale.
