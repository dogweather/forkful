---
title:                "Rédaction d'un fichier texte"
aliases:
- /fr/vba/writing-a-text-file/
date:                  2024-02-01T22:08:09.548942-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédaction d'un fichier texte"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/vba/writing-a-text-file.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire un fichier texte en Visual Basic pour Applications (VBA) implique de créer, modifier ou ajouter des données textuelles à des fichiers, une tâche fondamentale pour le stockage des sorties, la journalisation ou l'interaction avec d'autres applications. Les programmeurs utilisent cette fonctionnalité pour automatiser les rapports, l'exportation de données ou la génération de fichiers de configuration dans l'écosystème Microsoft Office.

## Comment faire :

VBA offre plusieurs méthodes pour écrire dans un fichier, mais l'une des manières les plus simples est d'utiliser le `FileSystemObject`. Voici un guide étape par étape pour créer un fichier texte simple et y écrire des données :

1. **Référencer Microsoft Scripting Runtime** : D'abord, assurez-vous que votre éditeur VBA a accès au `FileSystemObject`. Allez dans Outils > Références dans l'éditeur VBA et cochez "Microsoft Scripting Runtime."

2. **Créer un fichier texte** : Le fragment de code VBA suivant démontre comment créer un fichier texte et y écrire une ligne de texte.

```vb
Sub WriteToFile()
    Dim fso As FileSystemObject
    Set fso = New FileSystemObject
    
    Dim textFile As Object
    ' Paramètres de CreateTextFile : (NomDuFichier, Écraser, Unicode)
    Set textFile = fso.CreateTextFile("C:\votreChemin\exemple.txt", True, False)
    
    ' Écrire une ligne de texte
    textFile.WriteLine "Bonjour, VBA!"
    
    ' Fermer le fichier
    textFile.Close
End Sub
```

Ce script crée (ou écrase si déjà existant) un fichier nommé `exemple.txt` dans le répertoire spécifié et y écrit "Bonjour, VBA!" avant de fermer le fichier pour sauvegarder les modifications.

3. **Exemple de sortie** :

Après avoir exécuté le script VBA ci-dessus, vous trouverez un fichier nommé `exemple.txt` contenant le texte suivant :

```
Bonjour, VBA!
```

## Plongée profonde :

Le `FileSystemObject` (FSO), faisant partie de la librairie Microsoft Scripting Runtime, offre un riche ensemble de propriétés et méthodes pour les opérations sur les fichiers, allant au-delà de ce que le traitement classique de fichiers en VBA propose (par exemple, `Open`, `Print` #, `Write` #). En plus de gérer les fichiers, le FSO peut également manipuler les dossiers et les disques, le rendant un outil puissant pour les opérations sur le système de fichiers au sein de VBA.

Il est cependant important de noter que, bien que le FSO présente une approche plus moderne des opérations sur fichiers en VBA, il peut introduire une surcharge pour des tâches simples par rapport aux instructions de gestion de fichiers natives de VBA. De plus, étant donné que le FSO fait partie d'une librairie externe, la portabilité et la compatibilité avec d'autres systèmes (par exemple, les versions antérieures d'Office, Office sur Mac) pourraient être des préoccupations.

Dans des contextes où la performance, la compatibilité ou une dépendance minimale vis-à-vis des éléments externes sont critiques, les programmeurs peuvent envisager d'utiliser les techniques intégrées de gestion de fichiers de VBA. Cependant, pour des opérations plus complexes ou lorsqu'on travaille dans un environnement où ces préoccupations sont atténuées (comme un cadre d'entreprise contrôlé), les avantages du FileSystemObject l'emportent souvent sur ses inconvénients.
