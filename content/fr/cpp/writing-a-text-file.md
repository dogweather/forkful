---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"

category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Ecrire dans un fichier texte, c'est sauvegarder des données en format lisible. Les développeurs le font pour enregistrer des configurations, des logs ou des outputs de programmes.

## How to:
Pour écrire dans un fichier texte en C++, on utilise `ofstream` de la librairie `<fstream>`.

```C++
#include <iostream>
#include <fstream>

int main() {
    // Créer et ouvrir un fichier texte
    std::ofstream monFichier("exemple.txt");

    // Vérifier si le fichier est bien ouvert
    if (monFichier.is_open()) {
        // Ecrire dans le fichier
        monFichier << "Salut C++ World!" << std::endl;
        monFichier << "C'est facile d'écrire dans un fichier." << std::endl;

        // Fermer le fichier
        monFichier.close();
    } else {
        std::cout << "Impossible d'ouvrir le fichier." << std::endl;
    }

    return 0;
}
```

Ce code crée un fichier `exemple.txt` contenant:

```
Salut C++ World!
C'est facile d'écrire dans un fichier.
```

## Deep Dive
Avant `<fstream>`, on se servait de la librairie `<stdio.h>` (partie de C) avec `fprintf`. Des alternatives modernes incluent des librairies comme Boost.IOStreams pour plus de flexibilité.
Concernant l'implémentation, `ofstream` gère le buffer en arrière-plan, simplifiant l'écriture dans des fichiers.

## See Also
- C++ Reference on ofstream: https://en.cppreference.com/w/cpp/io/basic_ofstream
- C++ File I/O tutorial from cplusplus.com: http://www.cplusplus.com/doc/tutorial/files/
