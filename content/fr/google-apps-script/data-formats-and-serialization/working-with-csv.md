---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:18.600574-07:00
description: "Travailler avec des fichiers CSV (Comma-Separated Values ou Valeurs\
  \ S\xE9par\xE9es par des Virgules) dans Google Apps Script implique de lire, modifier\
  \ et\u2026"
lastmod: '2024-03-13T22:44:57.212958-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Comma-Separated Values ou Valeurs S\xE9\
  par\xE9es par des Virgules) dans Google Apps Script implique de lire, modifier et\
  \ \xE9crire des fichiers en texte brut o\xF9 chaque ligne repr\xE9sente un enregistrement\
  \ de donn\xE9es avec des valeurs s\xE9par\xE9es par des virgules."
title: Travailler avec CSV
weight: 37
---

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV (Comma-Separated Values ou Valeurs Séparées par des Virgules) dans Google Apps Script implique de lire, modifier et écrire des fichiers en texte brut où chaque ligne représente un enregistrement de données avec des valeurs séparées par des virgules. Les programmeurs font cela pour échanger facilement des données entre différentes applications, bases de données, ou langages de programmation grâce à l'adoption large du CSV comme format d'échange de données simple et basé sur le texte.

## Comment :

### Lire des Données CSV

Pour lire des données CSV depuis un fichier stocké dans Google Drive, vous devez d'abord obtenir le contenu du fichier sous forme de chaîne, puis l'analyser. Google Apps Script rend la récupération du contenu du fichier simple avec le service DriveApp.

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // Remplacer par l'ID réel du fichier
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Journaliser les cellules de chaque ligne
  }
}
```

### Écrire des Données CSV

Créer et écrire dans un CSV implique de construire une chaîne avec des valeurs séparées par des virgules et des sauts de ligne, puis de l'enregistrer ou de l'exporter. Cet exemple montre comment créer un nouveau fichier CSV dans Google Drive.

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // Remplacer par l'ID du dossier Drive où le nouveau fichier sera créé
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Exemple de Sortie

Lors de la journalisation des cellules de ligne lors de la lecture d'un CSV :

```plaintext
[John, 29, Engineer]
[Jane, 34, Designer]
```

Lors de l'écriture, un fichier nommé "example.csv" est créé avec le contenu :

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## Approfondissement

Historiquement, les fichiers CSV ont été favorisés pour leur simplicité et leur lisibilité par les humains, les rendant accessibles aux non-programmeurs et utiles pour des tâches d'inspection rapide des données. Cependant, Google Apps Script fonctionne dans l'univers de Google, où Google Sheets agit comme une alternative puissante et conviviale pour la manipulation de CSV. Sheets ne fournissent pas seulement une interface graphique pour éditer des données, mais prennent également en charge des formules complexes, le style, et bien d'autres fonctionnalités qui manquent aux CSV bruts.

Malgré les avantages offerts par Google Sheets, la manipulation directe des CSV dans Google Apps Script reste importante pour les tâches automatisées, surtout lorsqu'on traite avec des systèmes externes qui génèrent ou requièrent des données au format CSV. Par exemple, l'intégration avec des systèmes hérités, l'exportation de données pour utilisation dans d'autres applications, ou le prétraitement avant d'alimenter des données dans Google Sheets.

De plus, la capacité de Google Apps Script à travailler avec des fichiers CSV peut être étendue avec le service Utilities pour des besoins d'encodage avancés, ou interfacer avec des APIs externes pour des tâches de conversion, d'analyse, ou de validation. Cependant, pour travailler avec de grands ensembles de données ou nécessiter des manipulations complexes, envisagez d'utiliser les APIs Google Sheets ou d'explorer BigQuery pour des capacités de traitement de données plus robustes.

Alors que la simplicité reste une raison clé de la popularité du CSV, ces alternatives offrent un ensemble de fonctionnalités plus riche pour gérer des données dans l'écosystème étendu de Google Cloud.
