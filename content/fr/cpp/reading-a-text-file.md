---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lire un fichier texte en C++

## Qu'est-ce que c'est et Pourquoi?

Lire un fichier texte, c'est récupérer les données stockées dans ce fichier pour les utiliser dans votre programme. C'est essentiel car cela permet aux programmeurs d'accéder et de manipuler des informations enregistrées en dehors du code.

## Comment faire:

Voici comment lire un fichier texte en C++.

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream fichierTexte("fichier.txt"); //ouvre le fichier
    if (fichierTexte.is_open()) { //vérifie si le fichier est ouvert
        std::string ligne;
        while (getline(fichierTexte, ligne)) { //lit le fichier ligne par ligne
            std::cout << ligne << '\n'; //affiche chaque ligne
        }
        fichierTexte.close(); //ferme le fichier
    }
    else std::cout << "Impossible d'ouvrir le fichier";
    return 0;
}
```

La sortie de l'échantillon sera le contenu de votre fichier texte, affiché ligne par ligne.

## Plongée plus profonde

Historiquement, la lecture de fichiers textes a toujours été un aspect important de la programmation, du stockage de données au développement de systèmes de gestion de bases de données. 

Comme alternatives à la lecture de fichiers textes, nous avons les bases de données SQLite, MySQL, ainsi que les fichiers binaires - bien que les fichiers textes soient généralement plus faciles à utiliser et portables.

Lorsque nous lisons un fichier texte en C++, le fichier est ouvert en mode lecture, puis le flux d'entrée (`ifstream`) est utilisé pour lire les données. Les détails d'implémentation comprennent l'utilisation de la fonction 'getline' pour lire ligne par ligne, et l'utilisation de 'cout' pour l'affichage.

## Voir aussi

Pour plus d'informations sur la lecture de fichiers en C++, consultez ces liens utiles:

- Documentation officielle (https://en.cppreference.com/w/cpp/io)
- Guide pour débutants (https://www.guru99.com/cpp-file-read-write-open.html)
- Manière efficace de lire un grand fichier texte (https://stackoverflow.com/questions/17925051/fast-textfile-reading-in-c)

N'oubliez pas, pratiquer et expérimenter avec de vrais problèmes de programmation sont les meilleurs moyens d'apprendre.