---
title:    "C++: Lecture d'un fichier texte"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire un fichier texte peut sembler être une tâche simple et banale, mais cela peut être une compétence très utile pour les programmeurs. Que vous souhaitiez récupérer des données à partir d'un fichier ou manipuler des informations spécifiques, savoir comment lire un fichier texte peut vous faire gagner du temps et faciliter votre travail de programmation.

## Comment faire

Pour lire un fichier texte en utilisant le langage de programmation C++, vous pouvez suivre ces étapes simples :

1. Tout d'abord, utilisez la fonction `ifstream` pour ouvrir et lire le fichier. Par exemple, vous pouvez utiliser `ifstream file("monfichier.txt");` pour ouvrir le fichier `monfichier.txt`.
2. Ensuite, utilisez une boucle `while` pour lire le fichier jusqu'à la fin. Vous pouvez utiliser la fonction `getline()` pour lire une ligne complète du fichier à la fois.
3. Utilisez une variable de type `string` pour stocker chaque ligne lue du fichier.
4. Vous pouvez ensuite utiliser les données lues pour effectuer des manipulations ou les afficher à l'écran.

Voici un exemple de code montrant comment lire un fichier texte et afficher son contenu à l'écran :

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    // Crée une instance de ifstream pour ouvrir le fichier
    ifstream fichier("monfichier.txt");

    // Variable pour stocker chaque ligne lue du fichier
    string ligne;

    // Boucle pour lire le fichier jusqu'à la fin
    while (getline(fichier, ligne)) {
        // Affiche la ligne lue à l'écran
        cout << ligne << endl;
    }

    // Ferme le fichier
    fichier.close();

    return 0;
}
```

Voici un exemple de sortie pour un fichier `monfichier.txt` contenant les lignes "Bonjour" et "Comment ça va ?" :

```text
Bonjour
Comment ça va ?
```

## Plongée plus profonde

En plus de simplement lire un fichier texte, il est également possible de lire des données spécifiques à partir de celui-ci en utilisant des fonctions comme `find()` et `substr()`. Ces fonctions vous permettent de rechercher et extraire des informations précises, en utilisant par exemple des caractères ou des motifs spécifiques.

Il est également important de noter que lors de la lecture d'un fichier texte, il est possible de rencontrer des problèmes de caractères spéciaux ou de sauts de ligne. Cela peut être résolu en utilisant des fonctions de manipulation de chaines de caractères et en gérant soigneusement les données lues.

## Voir aussi

- [Documentation officielle pour `ifstream` en C++](https://www.cplusplus.com/reference/fstream/ifstream/)
- [Tutoriel vidéo sur la lecture de fichiers texte en C++](https://www.youtube.com/watch?v=Iho2EdJgusQ)
- [Exemples de projets utilisant la lecture de fichiers en C++](https://www.programiz.com/cpp-programming/examples/read-write-file)