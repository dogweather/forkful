---
title:    "C++: Vérification de l'existence d'un répertoire"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Les vérifications des répertoires sont une partie essentielle de la programmation en C++. Elles permettent aux développeurs de s'assurer que les fichiers et les dossiers nécessaires existent avant d'exécuter certaines fonctions ou tâches. Cela peut aider à éviter des erreurs et des bugs potentiels dans le code.

## Comment faire

Pour vérifier si un répertoire existe en utilisant C++, vous pouvez utiliser la fonction `opendir()` de la bibliothèque `<dirent.h>`. Cette fonction ouvre un répertoire et renvoie un pointeur de type `DIR *` (directory stream) s'il réussit, ou `NULL` s'il échoue.

Exemple de code :

```C++
#include <dirent.h>
using namespace std;

DIR *dir = opendir("nom_du_repertoire"); // Remplacez "nom_du_repertoire" par le nom de votre répertoire
if (dir) {
    cout << "Le repertoire existe." << endl;
    closedir(dir); // Ferme le répertoire ouvert
} else {
    cout << "Le repertoire n'existe pas." << endl;
}
```

Exemple de sortie :

```
Le repertoire existe.
```

## Plonger plus profondément

En plus de la fonction `opendir()`, il existe d'autres moyens de vérifier l'existence d'un répertoire en utilisant C++. Vous pouvez utiliser la fonction `stat()` de la bibliothèque `<sys/stat.h>` qui renvoie des informations sur le fichier ou le répertoire spécifié. Si le répertoire n'existe pas, la fonction renverra une erreur.

Exemple de code :

```C++
#include <sys/stat.h>
using namespace std;

struct stat file_info;
if (stat("chemin_du_repertoire", &file_info) == 0) { // Remplacez "chemin_du_repertoire" par le chemin complet de votre répertoire
    if (file_info.st_mode & S_IFDIR) {
        cout << "Le repertoire existe." << endl;
    } else {
        cout << "Ce n'est pas un repertoire." << endl;
    }
} else {
    cout << "Le repertoire n'existe pas." << endl;
}
```

Exemple de sortie :

```
Le repertoire existe.
```

## Voir aussi

- [Documentation C++ pour opendir](http://www.cplusplus.com/reference/cstdio/opendir/)
- [Documentation C++ pour stat](http://www.cplusplus.com/reference/cstdio/stat/) 
- [Tutoriel sur l'utilisation de opendir et stat en C++](https://www.programiz.com/cpp-programming/library-function/dirent/stat)