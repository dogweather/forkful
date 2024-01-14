---
title:    "C++: Lecture d'un fichier texte"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez en C++, il peut arriver que vous ayez besoin de lire un fichier texte pour utiliser son contenu dans votre code. Dans cet article, nous allons vous expliquer comment faire cela de manière simple et efficace.

## Comment faire

Pour lire un fichier texte en C++, nous allons utiliser la bibliothèque standard <fstream>. Tout d'abord, nous allons ouvrir le fichier en utilisant la fonction open() avec le nom du fichier en paramètre. Ensuite, nous pourrons lire le contenu du fichier en utilisant la fonction getline() et en stockant chaque ligne dans une variable. Voici un exemple de code :

```C++
#include <fstream>
#include <iostream>

using namespace std;

int main()
{
    // Ouvrir le fichier en mode lecture
    ifstream fichier("mon_fichier.txt");
    
    // Vérifier si le fichier est ouvert
    if (fichier.is_open()) {
        // Lire et afficher chaque ligne du fichier
        string ligne;
        while(getline(fichier, ligne)) {
            cout << ligne << endl;
        }
        
        // Fermer le fichier
        fichier.close();
    } else {
        // Afficher un message d'erreur si le fichier ne peut pas être ouvert
        cout << "Erreur lors de l'ouverture du fichier" << endl;
    }
    
    return 0;
}
```

Supposons que notre fichier texte soit le suivant :

```
Bonjour
Comment ça va ?
Je suis un fichier texte.
```

Lorsque nous exécutons notre programme, nous obtiendrons en sortie :

```
Bonjour
Comment ça va ?
Je suis un fichier texte.
```

## Plongée en profondeur

Il est important de noter que la fonction getline() lit une ligne à la fois. Si nous avons besoin de parcourir le fichier ligne par ligne, nous pouvons utiliser une boucle while comme dans notre exemple, ou bien utiliser une boucle for et la fonction eof() pour vérifier si le fichier est arrivé à sa fin. De plus, la fonction getline() peut également prendre un délimiteur en paramètre, ce qui peut être utile si nous voulons extraire des données spécifiques d'une ligne. Pour plus d'informations sur ces fonctions et sur la lecture de fichiers en général, vous pouvez consulter la documentation officielle de C++.

## Voir aussi

- [Documentation officielle de C++ sur la lecture de fichiers](https://en.cppreference.com/w/cpp/io/basic_ifstream)
- [Tutoriel sur la lecture et l'écriture de fichiers en C++](https://www.cplusplus.com/doc/tutorial/files/)