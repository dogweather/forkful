---
title:                "C++: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut être très utile de savoir comment lire un fichier texte en C++. Il est courant d'avoir à travailler avec des fichiers texte dans des projets de programmation, en particulier lorsqu'il s'agit de manipuler de grandes quantités de données. Apprendre à lire un fichier texte vous permettra de comprendre comment accéder et manipuler ces données, ce qui peut améliorer considérablement vos compétences en programmation.

## Comment Faire

Voici un exemple simple de code en C++ pour lire un fichier texte :

```C++
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main()
{
    // Déclaration du fichier et ouverture
    ifstream file("exemple.txt");

    // Vérifie si le fichier est ouvert avec succès
    if (!file.is_open())
    {
        cout << "Erreur lors de l'ouverture du fichier !" << endl;
        return 0;
    }

    // Lecture ligne par ligne
    string line;
    while (getline(file, line))
    {
        cout << line << endl;
    }

    // Fermeture du fichier
    file.close();

    return 0;
}
```

Supposons que le fichier texte que nous lisons contient ces lignes :

```
Bonjour !
Comment ça va?
Bonne journée!
```

La sortie sera :

```
Bonjour !
Comment ça va?
Bonne journée!
```

## Plongée Plus Profonde

Maintenant que nous avons vu un exemple simple de lecture d'un fichier texte, il est temps de plonger plus profondément dans le sujet. Voici quelques éléments à considérer lorsque vous travaillez avec des fichiers texte en C++ :

- Les fichiers texte sont généralement ouverts en mode "lecture seule" par défaut. Cela signifie que vous ne pourrez pas modifier le contenu du fichier.
- Si vous souhaitez écrire dans un fichier texte, vous devrez ouvrir le fichier en mode "écriture" ou "lecture/écriture".
- Il est important de vérifier si le fichier est ouvert avec succès avant de commencer à le lire ou à l'écrire.
- La fonction `getline` lit une ligne entière du fichier et stocke son contenu dans une variable de type string.
- Vous pouvez également utiliser la fonction `get` pour lire un caractère à la fois du fichier.
- N'oubliez pas de fermer le fichier après avoir fini de le lire ou de l'écrire.

En utilisant ces informations, vous pourrez lire et manipuler des fichiers texte avec confiance et efficacité en C++.

## Voir Aussi

- [Documentation officielle sur les fichiers en C++](https://en.cppreference.com/w/cpp/io)
- [Tutoriel sur la manipulation de fichiers en C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Exemples de projets utilisant la lecture de fichiers en C++](https://github.com/topics/cpp-file-handling)