---
title:                "Vérifier si un répertoire existe"
aliases:
- /fr/google-apps-script/checking-if-a-directory-exists/
date:                  2024-02-01T21:49:04.897226-07:00
model:                 gpt-4-0125-preview
simple_title:         "Vérifier si un répertoire existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Vérifier si un répertoire existe dans Google Apps Script implique de vérifier la présence d'un dossier au sein de Google Drive. Les programmeurs effectuent souvent cette vérification pour éviter les erreurs ou la création de dossiers redondants lors de la gestion des fichiers et des répertoires de manière programmatique.

## Comment faire :

Google Apps Script n'offre pas de méthode "exists" directe pour les dossiers. Au lieu de cela, nous utilisons les capacités de recherche de Google Drive pour vérifier si un dossier portant un nom spécifique existe. Voici un exemple étape par étape :

```javascript
// Fonction pour vérifier si un répertoire existe
function checkIfDirectoryExists(directoryName) {
  // Récupérer la collection de dossiers correspondant au nom spécifié
  var folders = DriveApp.getFoldersByName(directoryName);
  
  // Vérifier si au moins un dossier portant le nom spécifié existe
  if (folders.hasNext()) {
    Logger.log('Le répertoire existe.');
    return true;
  } else {
    Logger.log('Le répertoire n'existe pas.');
    return false;
  }
}

// Exemple d'utilisation
var directoryName = 'Mon Dossier Exemple';
checkIfDirectoryExists(directoryName);
```

Exemple de sortie :
```
Le répertoire existe.
```
ou 
```
Le répertoire n'existe pas.
```

Ce script utilise la méthode `getFoldersByName` qui récupère tous les dossiers dans le Drive de l'utilisateur correspondant au nom spécifié. Étant donné que les noms ne sont pas uniques dans Drive, cette méthode retourne un `FolderIterator`. La présence d'un prochain élément (`hasNext()`) dans cet itérateur indique que le répertoire existe.

## Approfondissement

Historiquement, la gestion des fichiers dans les environnements web et cloud a évolué de manière significative. Google Apps Script, fournissant une API extensive pour Google Drive, permet des opérations de gestion sophistiquées de fichiers et de dossiers, y compris les mécanismes de recherche et de vérification démontrés. Cependant, un aspect notable est l'absence de vérification directe de l'existence, probablement dû à l'autorisation par Google Drive de multiples dossiers portant le même nom, ce qui contraste avec de nombreux systèmes de fichiers qui imposent des noms uniques dans le même répertoire.

Dans ce contexte, utiliser la méthode `getFoldersByName` est une solution de contournement efficace mais pourrait potentiellement introduire des inefficacités dans un scénario où de nombreux dossiers portant des noms dupliqués existent. Une approche alternative pourrait impliquer de maintenir une indexation ou une convention de nommage spécifique à l'application pour garantir des vérifications plus rapides, en particulier lorsque la performance devient une préoccupation critique.

Bien que l'approche de Google Apps Script puisse initialement sembler moins directe par rapport aux vérifications de l'existence des fichiers dans les langages de programmation directement interférés avec un système de fichiers singulier, elle reflète la nécessité de gérer les complexités du stockage de fichiers basé sur le cloud. Les développeurs exploitant Google Apps Script pour la gestion de Drive devraient prendre en compte ces nuances, optimisant pour les forces et limitations de Google Drive.
