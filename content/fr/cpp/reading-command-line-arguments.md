---
title:                "C++: Lecture des arguments en ligne de commande"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant ou expérimenté, vous connaissez sûrement l'importance des arguments de ligne de commande dans vos programmes. Ceux-ci vous permettent de fournir des informations à votre programme lors de son exécution, sans avoir à les définir directement dans votre code. Dans cet article, nous allons explorer comment lire ces arguments de ligne de commande en utilisant C++. 

## Comment faire

Tout d'abord, il est important de noter que les arguments de ligne de commande sont essentiellement une liste de chaînes de caractères, séparées par des espaces, qui sont passées à votre programme lors de son exécution. Pour les lire en utilisant C++, nous pouvons utiliser les paramètres de la fonction `main` qui prennent la forme `int argc, char* argv[]`. L'argument `argc` représente le nombre total d'arguments passés, y compris le nom du programme lui-même, tandis que `argv` est un tableau de chaînes de caractères représentant chaque argument passé.

Voici un exemple de code pour lire et afficher les arguments de ligne de commande :

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    // Boucle à travers chaque argument
    for (int i = 0; i < argc; i++) {
        // Affiche l'index de l'argument et sa valeur
        std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }

    return 0;
}
```

Si nous exécutons ce programme avec les arguments "Bonjour tout le monde", nous obtiendrons la sortie suivante :

```
Argument 0: ./programme
Argument 1: Bonjour
Argument 2: tout
Argument 3: le
Argument 4: monde
```

Comme vous pouvez le voir, l'argument 0 représente le nom du programme lui-même, suivi de chaque mot passé en tant qu'argument. Il est important de noter que les arguments sont toujours lus en tant que chaînes de caractères, vous devrez donc peut-être les convertir en d'autres types de données en fonction de vos besoins.

## Plongée en profondeur

En plus de lire simplement les arguments de ligne de commande, il y a quelques astuces utiles à connaître lors de leur utilisation dans vos programmes. Tout d'abord, vous pouvez utiliser `std::stoi` pour convertir une chaîne de caractères en un entier, ou `std::stod` pour un nombre à virgule flottante. Deuxièmement, vous pouvez utiliser les bibliothèques de traitement de chaînes de caractères telles que `#include <string>` et `#include <sstream>` pour manipuler et extraire des informations à partir des arguments. Et enfin, vous pouvez également utiliser des drapeaux ou des options pour fournir des arguments optionnels à votre programme.

## Voir aussi

- [Documentation C++ sur les arguments de ligne de commande](https://en.cppreference.com/w/cpp/utility/program/argument_vector)
- [Guide pratique pour lire les arguments de ligne de commande en C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Tutoriel en vidéo sur l'utilisation des arguments de ligne de commande en C++](https://www.youtube.com/watch?v=KszBqGGnITI)