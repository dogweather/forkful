---
title:                "Création d'un fichier temporaire"
aliases:
- /fr/google-apps-script/creating-a-temporary-file.md
date:                  2024-02-01T21:51:33.720196-07:00
model:                 gpt-4-0125-preview
simple_title:         "Création d'un fichier temporaire"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/creating-a-temporary-file.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La création d'un fichier temporaire dans Google Apps Script consiste à générer un fichier destiné à un usage à court terme, typiquement pour le traitement intermédiaire des données, le débogage ou des fins de cache. Les programmeurs font cela pour gérer temporairement des données sans encombrer l'espace de stockage permanent ou lorsque la pérennité des données n'est pas nécessaire au-delà de la portée du processus en cours.

## Comment faire :

Dans Google Apps Script, créer un fichier temporaire peut être réalisé en utilisant le service DriveApp, qui fournit une méthode simple pour créer, lire et supprimer des fichiers dans Google Drive. Voici comment vous pouvez créer un fichier texte temporaire, y écrire des données, puis le supprimer après utilisation :

```javascript
function createTemporaryFile() {
  // Crée un fichier temporaire nommé "tempFile.txt"
  var tempFile = DriveApp.createFile('tempFile.txt', 'Contenu temporaire', MimeType.PLAIN_TEXT);
  
  // Enregistre l'URL du fichier pour accès ou débogage
  Logger.log('Fichier temporaire créé : ' + tempFile.getUrl());
  
  // Opération exemple : Lire le contenu du fichier
  var content = tempFile.getBlob().getDataAsString();
  Logger.log('Contenu de tempFile : ' + content);
  
  // En supposant que l'opération est complète et que le fichier n'est plus nécessaire
  // Supprime le fichier temporaire
  tempFile.setTrashed(true);
  
  // Confirme la suppression
  Logger.log('Fichier temporaire supprimé');
}
```

L'exécution de ce script produirait :

```
Fichier temporaire créé : [URL du fichier temporaire créé]
Contenu de tempFile : Contenu temporaire
Fichier temporaire supprimé
```

Ce script exemple montre la création d'un fichier temporaire, l'exécution d'une opération pour lire son contenu et enfin, la suppression du fichier pour nettoyer.

## Exploration Approfondie

Le concept de création de fichiers temporaires dans le développement logiciel est aussi ancien que le concept de gestion des fichiers lui-même. Dans les systèmes de fichiers traditionnels, les fichiers temporaires sont souvent créés dans des répertoires temp désignés et sont cruciaux pour divers processus intermédiaires, comme le tri de grands ensembles de données, la conservation des données de session pour les applications web ou le stockage de segments de données pendant les processus de conversion de fichiers.

Dans Google Apps Script, le processus de création de fichiers temporaires tire profit de l'infrastructure de Google Drive, qui offre un mélange intéressant de gestion de fichiers basée sur le cloud avec des concepts de programmation traditionnels. Cependant, cette méthode de création de fichiers temporaires dans Google Drive n’est pas sans limitations et coûts, compte tenu des limites de quota imposées par Google Drive. De plus, la latence d'accès à Google Drive sur le réseau par rapport à un système de fichiers local peut être un facteur critique pour les applications à haute performance.

Comme alternatives, les développeurs pourraient envisager d'utiliser Google Sheets pour les petits ensembles de données qui nécessitent un stockage temporaire pendant le calcul, ou Google Cloud Storage pour les applications qui exigent des opérations de lecture/écriture à haute performance et des capacités de stockage plus importantes. Chacune de ces solutions offre différents compromis en termes de latence, de limites de stockage et de facilité d'utilisation depuis Google Apps Script. En fin de compte, le choix dépend des exigences spécifiques de l'application et de l'infrastructure existante au sein de laquelle elle fonctionne.
