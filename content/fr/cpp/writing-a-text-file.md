---
title:                "Écrire un fichier texte"
html_title:           "C++: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Ecrire un fichier texte signifie créer un document sous forme de texte brut au lieu d'utiliser un formatage avancé comme dans les traitements de texte. Les programmateurs le font souvent pour stocker et manipuler des données de manière simple et lisible par les humains et les machines.

## Comment faire:
Pour écrire un fichier texte en C++, vous devez suivre ces étapes:
- Ouvrez un flux de sortie vers un fichier en utilisant l'objet `ofstream` et le nom du fichier en paramètre.
- Vérifiez si le fichier est ouvert avec la méthode `is_open()` de l'objet `ofstream`.
- Utilisez l'opérateur de flux `<<` pour écrire le contenu dans le fichier, comme dans l'exemple ci-dessous:
```C++
ofstream file("mon_fichier.txt");//ouverture du flux vers le fichier
file << "Bonjour le monde!" << endl;//écriture du contenu dans le fichier
file.close();//fermeture du flux
```
- Assurez-vous de fermer le flux une fois que vous avez fini d'écrire dans le fichier en utilisant la méthode `close()`.

Le contenu du fichier sera alors:
```
Bonjour le monde!
```

## Plongée en profondeur:
L'écriture de fichiers texte en C++ a été introduite pour la première fois dans la version 2.0 du langage en 1985, avec l'utilisation des classes `fstream`. Les alternatives à cette méthode incluent l'utilisation de fonctions C telles que `fopen()` et `fprintf()`, ainsi que des bibliothèques tierces comme Qt.

L'implémentation de l'écriture de fichiers textes en C++ est basée sur la surcharge des opérateurs de flux `<<`, qui permettent d'écrire des données dans un flux de sortie. Les méthodes `ofstream::open()` et `ofstream::write()` sont également utilisées pour écrire des données dans un fichier.

## Voir aussi:
- Documentation officielle C++ sur l'écriture de fichiers: https://en.cppreference.com/w/cpp/io/basic_ofstream
- Tutoriel en français sur l'écriture de fichiers en C++: https://www.firmcodes.com/fr/c-2/c-ecriture-de-fichiers-en-c/