---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:55.831980-07:00
description: "Comment : Cr\xE9er et \xE9crire dans un fichier texte avec Google Apps\
  \ Script peut \xEAtre accompli gr\xE2ce au service Google DriveApp. Voici un guide\
  \ \xE9tape par\u2026"
lastmod: '2024-03-13T22:44:57.208343-06:00'
model: gpt-4-0125-preview
summary: "Cr\xE9er et \xE9crire dans un fichier texte avec Google Apps Script peut\
  \ \xEAtre accompli gr\xE2ce au service Google DriveApp."
title: "R\xE9daction d'un fichier texte"
weight: 24
---

## Comment :
Créer et écrire dans un fichier texte avec Google Apps Script peut être accompli grâce au service Google DriveApp. Voici un guide étape par étape avec des exemples de code pour vous aider à démarrer :

**Étape 1 : Créer un nouveau fichier texte**

```javascript
// Crée un nouveau fichier texte à la racine de Google Drive
var file = DriveApp.createFile('Example.txt', 'Bonjour, monde !');
```

Ce fragment de code crée un fichier texte intitulé "Example.txt" avec le contenu "Bonjour, monde !".

**Étape 2 : Ouvrir et écrire dans un fichier texte existant**

Si vous devez ouvrir un fichier existant et y écrire, vous pouvez utiliser la méthode `getFileById(id)` pour récupérer le fichier et ensuite manipuler son contenu.

```javascript
// Obtient un fichier par son ID et ajoute un nouveau contenu
var fileId = 'VOTRE_ID_DE_FICHIER_ICI'; // Remplacez VOTRE_ID_DE_FICHIER_ICI par votre véritable ID de fichier
var file = DriveApp.getFileById(fileId);
file.setContent(file.getBlob().getDataAsString() + '\nNouveau contenu ajouté.');
```

Ce code récupère un fichier existant en utilisant son ID unique, puis ajoute "Nouveau contenu ajouté." à tout contenu qui était précédemment là.

**Exemple de sortie**

Aucune sortie explicite n'est affichée en exécutant les fragments de code ci-dessus, mais si vous naviguez vers le Google Drive où le fichier est situé, vous verrez "Example.txt" pour le premier fragment de code. Pour le deuxième fragment, si vous ouvrez le fichier spécifié par ID, vous devriez voir le contenu original suivi par la nouvelle ligne "Nouveau contenu ajouté."

## Plongée profonde
Écrire un fichier texte dans Google Apps Script tire parti du service DriveApp, exploitant essentiellement les capacités de Google Drive pour le stockage et la gestion de fichiers. Cette approche remonte à la création de Google Apps Script, qui a été conçu pour automatiser facilement des tâches à travers la suite d'outils de productivité de Google, y compris Drive.

Bien que la manipulation directe de fichiers via Google Apps Script soit simple et étroitement intégrée à Google Workspace, les développeurs venant d'autres horizons (par exemple, Python, Node.js) pourraient la trouver différente de travailler avec un système de fichiers local ou d'autres services de stockage en nuage comme AWS S3. Ces plateformes offrent souvent un ensemble de capacités de manipulation de fichiers plus complexes, mais nécessitent une configuration supplémentaire pour l'authentification et les permissions.

Pour les scénarios nécessitant des capacités de gestion ou de traitement de fichiers plus avancées au-delà des simples fichiers texte (comme la gestion de données binaires ou les opérations étendues sur le système de fichiers), les développeurs pourraient envisager d'utiliser les services de Google Cloud Platform (par exemple, Cloud Storage) en conjonction avec Google Apps Script. Ces alternatives, bien que plus puissantes, introduisent également une courbe d'apprentissage plus abrupte et des coûts potentiellement plus élevés, en fonction de la portée du projet.

En conclusion, bien que Google Apps Script offre un moyen accessible et efficace de gérer les fichiers au sein de Google Drive, y compris l'écriture de fichiers texte, il est important de comprendre ses limites et d'explorer d'autres technologies Google selon les besoins pour répondre à des exigences plus complexes.
