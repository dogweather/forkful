---
title:    "C++: Écrire un fichier texte"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles écrire un fichier texte peut être utile en programmation. Tout d'abord, les fichiers texte sont un moyen efficace de stocker de grandes quantités de données, sans avoir à les saisir manuellement à chaque utilisation. De plus, il est souvent plus facile de manipuler les informations contenues dans un fichier texte plutôt que de travailler avec des données en direct dans votre programme.

## Comment faire

Pour écrire un fichier texte en C++, vous aurez besoin de la bibliothèque `fstream`. Vous pouvez ouvrir un fichier texte en utilisant la fonction `open()` en spécifiant le nom du fichier et le mode d'ouverture. Voici un exemple:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    ofstream fichier("mon_fichier.txt");

    // Si le fichier n'existe pas, il sera créé
    fichier << "Ceci est un exemple de texte." << endl;

    fichier.close(); // Toujours fermer le fichier une fois que vous avez fini de l'utiliser

    return 0;
}
```

Dans cet exemple, nous avons créé un objet `ofstream` appelé `fichier` en lui donnant le nom de notre fichier `mon_fichier.txt` en argument. Ensuite, nous avons utilisé l'opérateur `<<` pour écrire une chaîne de caractères dans notre fichier, suivie de `endl` pour ajouter une nouvelle ligne. Enfin, n'oubliez pas de fermer le fichier en appelant la fonction `close()`.

Vous pouvez également spécifier un mode différent pour ouvrir un fichier, comme le mode `ios::app` pour ajouter du contenu à la fin du fichier ou `ios::trunc` pour vider le fichier avant d'écrire dedans.

## Plongée en profondeur

En plus des opérations de base que nous avons vues dans la section précédente, il existe des fonctionnalités avancées pour lire et écrire dans des fichiers en utilisant la bibliothèque `fstream`. Par exemple, vous pouvez déplacer le pointeur de position dans votre fichier en utilisant la fonction `seekp()` pour écrire à un endroit spécifique ou `seekg()` pour lire à partir d'un endroit spécifique. De plus, vous pouvez également utiliser la fonction `tellp()` pour connaître la position actuelle du pointeur d'écriture ou `tellg()` pour connaître la position actuelle du pointeur de lecture.

Il est également important de noter que tout ce que vous écrivez dans un fichier texte sera converti en caractères. Cela signifie que si vous souhaitez écrire des données numériques dans un fichier, vous devrez les convertir en chaîne de caractères avant de les écrire.

## Voir aussi

- [Cppreference - std::ofstream](https://en.cppreference.com/w/cpp/io/basic_ofstream)
- [Cours de programmation en C++ - Les fichiers](https://www.cplusplus.com/doc/tutorial/files/)