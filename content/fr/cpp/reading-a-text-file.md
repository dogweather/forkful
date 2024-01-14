---
title:                "C++: La lecture d'un fichier texte"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire un fichier texte est une opération courante dans la programmation en C++. Cela permet de stocker et de récupérer des données à partir d'un fichier externe, ce qui peut être utile pour des tâches telles que la sauvegarde de données ou le traitement de fichiers de configuration. Dans cet article, nous allons explorer comment lire un fichier texte en C++ et pourquoi cela peut être utile.

## Comment procéder

Pour lire un fichier texte en C++, nous devons suivre quelques étapes simples:

1. Inclure la bibliothèque `<fstream>`, qui contient les fonctions pour la manipulation de fichiers.
2. Ouvrir un objet de type `ifstream` en utilisant la fonction `open()` et en spécifiant le nom du fichier que l'on souhaite lire.
3. Utiliser la fonction `getline()` pour lire les lignes du fichier et les stocker dans une chaîne de caractères.
4. Fermer l'objet `ifstream` à la fin de la lecture du fichier.

Un exemple de code pour lire un fichier texte appelé "data.txt" pourrait ressembler à ceci:

```C++
#include <fstream>
#include <iostream>

using namespace std;

int main() {
    // Ouvrir le fichier texte en lecture
    ifstream fichier("data.txt");

    // Vérifier si le fichier a bien été ouvert
    if (fichier.is_open()) {
        string ligne;

        // Lire et afficher chaque ligne du fichier
        while (getline(fichier, ligne)) {
            cout << ligne << endl;
        }

        // Fermer le fichier
        fichier.close();
    }
    else {
        // Afficher un message d'erreur si le fichier n'a pas été ouvert
        cout << "Erreur lors de l'ouverture du fichier." << endl;
    }

    return 0;
}
```

Supposons que le fichier "data.txt" contienne les lignes suivantes:

```
Bonjour
Comment ça va?
Je m'appelle Jean.
```

L'exemple ci-dessus affichera:

```
Bonjour
Comment ça va?
Je m'appelle Jean.
```

## Approfondissement

Maintenant que nous avons vu comment lire un fichier texte en C++, il est important de comprendre que les données lues seront stockées dans un objet de type `string`. De plus, en utilisant la fonction `getline()`, nous lisons le fichier ligne par ligne, ce qui peut être utile si le fichier contient des données organisées en lignes.

Il est également important de noter que la lecture d'un fichier texte en C++ peut être un processus plus complexe si le fichier contient des données de différents types, tels que des nombres ou des caractères spéciaux. Dans ce cas, il est souvent nécessaire d'utiliser des fonctions de conversion de type pour manipuler les données lues.

Enfin, il est recommandé de toujours vérifier si le fichier a bien été ouvert avant de commencer à lire ou à écrire des données. Cela évite les erreurs et les plantages du programme.

## Voir aussi

- [Documentation sur la lecture et l'écriture de fichiers en C++](https://www.cplusplus.com/doc/tutorial/files/)
- [Tutoriel vidéo sur la lecture de fichiers en C++](https://youtu.be/w41pTGtH0Uw)