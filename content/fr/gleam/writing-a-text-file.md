---
title:                "Ecrire un fichier texte"
html_title:           "Gleam: Ecrire un fichier texte"
simple_title:         "Ecrire un fichier texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?
Écrire un fichier texte est une tâche courante pour les programmeurs, car cela leur permet de stocker et de transférer des données d'un programme à un autre. Les fichiers texte peuvent être lus et écrits par n'importe quel éditeur de texte, les rendant faciles à utiliser et à partager.

# Comment faire?
Voici un exemple de code pour écrire une ligne de texte dans un fichier texte en utilisant Gleam:

```Gleam
// Création d'un fichier texte et ouverture en écriture
let mon_fichier = File.append("mon_fichier.txt");
// Écriture d'une ligne de texte
File.write_line(mon_fichier, "Voici une ligne de texte.");
// Fermeture du fichier
File.close(mon_fichier);

// Lecture du fichier pour vérifier le résultat
let contenu = File.read("mon_fichier.txt");
```

Le contenu du fichier "mon_fichier.txt" sera "Voici une ligne de texte.".

# Plongée en profondeur
Les fichiers texte ont été utilisés pendant des décennies pour stocker des données ou du code dans un format lisible par l'homme. Les alternatives à l'utilisation de fichiers texte incluent les bases de données, qui peuvent offrir des fonctionnalités plus avancées mais peuvent être plus complexes à mettre en place. Pour écrire un fichier texte en Gleam, la fonction `File.write_line` utilise une bibliothèque sous-jacente appelée "File I/O", qui est basée sur les API de système de fichiers POSIX.

# À voir aussi
- Documentation de la bibliothèque de fichiers Gleam : https://gleam.run/modules/gleam/files.html
- Tutoriel sur la lecture et l'écriture de fichiers en Gleam : https://gleam.run/articles/example_8.html